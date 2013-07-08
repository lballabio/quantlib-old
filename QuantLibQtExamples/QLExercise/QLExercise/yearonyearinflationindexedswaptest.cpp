/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2009 Chris Kenyon

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

#include "yearonyearinflationindexedswaptest.hpp"
#include "customutilities.hpp"

#include <ql/math/interpolations/cubicinterpolation.hpp>
#include <ql/math/interpolations/bicubicsplineinterpolation.hpp>
#include <ql/termstructures/yield/zerocurve.hpp>
#include <ql/termstructures/inflation/interpolatedyoyinflationcurve.hpp>

#include <ql/cashflows/inflationcoupon.hpp>
#include <ql/cashflows/inflationcouponpricer.hpp>

#include <ql/experimental/inflation/yoycapfloortermpricesurface.hpp>
#include <ql/pricingengines/inflation/inflationcapfloorengines.hpp>
#include <ql/experimental/inflation/yoyoptionletstripper.hpp>

#include <ql/experimental/inflation/kinterpolatedyoyoptionletvolatilitysurface.hpp>
#include <ql/experimental/inflation/interpolatedyoyoptionletstripper.hpp>

#include <ql/cashflows/capflooredinflationcoupon.hpp>
#include <ql/indexes/inflation/euhicp.hpp>
#include <ql/indexes/inflation/ukrpi.hpp>

// added
#include <ql/cashflows/cpicoupon.hpp>
#include <ql/time/calendars/unitedkingdom.hpp>
#include <ql/instruments/yearonyearinflationswap.hpp>
#include <ql/time/daycounters/actualactual.hpp>
#include <ql/termstructures/inflation/piecewisezeroinflationcurve.hpp>
#include <ql/pricingengines/swap/discountingswapengine.hpp>

#include <iostream>


// anonymous local namespace for data
//*************************************************************************
namespace {

	using namespace std;
	using namespace boost;
	using namespace QuantLib;

	void no_deletion(void*) {}

	// local data globals
	Handle<YieldTermStructure> nominalEUR;
	Handle<YieldTermStructure> nominalGBP;

	RelinkableHandle<YoYInflationTermStructure> yoyEU;
	RelinkableHandle<YoYInflationTermStructure> yoyUK;

	vector<Rate> cStrikesEU;
	vector<Rate> fStrikesEU;
	vector<Period> cfMaturitiesEU;
	boost::shared_ptr<Matrix> cPriceEU;
	boost::shared_ptr<Matrix> fPriceEU;

	boost::shared_ptr<YoYInflationIndex> yoyIndexUK;
	boost::shared_ptr<YoYInflationIndex> yoyIndexEU;

	vector<Rate> cStrikesUK;
	vector<Rate> fStrikesUK;
	vector<Period> cfMaturitiesUK;
	boost::shared_ptr<Matrix> cPriceUK;
	boost::shared_ptr<Matrix> fPriceUK;

	boost::shared_ptr<InterpolatedYoYCapFloorTermPriceSurface<Bicubic,Cubic> > priceSurfEU;

	void reset() {
		nominalEUR = Handle<YieldTermStructure>();
		nominalGBP = Handle<YieldTermStructure>();
		priceSurfEU.reset();
		yoyEU.linkTo(boost::shared_ptr<YoYInflationTermStructure>());
		yoyUK.linkTo(boost::shared_ptr<YoYInflationTermStructure>());
		yoyIndexUK.reset();
		yoyIndexEU.reset();
		cPriceEU.reset();
		fPriceEU.reset();
		cPriceUK.reset();
		fPriceUK.reset();
		yoyIndexUK.reset();

		cStrikesEU.clear();        
		fStrikesEU.clear();
		cStrikesUK.clear();        
		fStrikesUK.clear();
		cfMaturitiesEU.clear();
		cfMaturitiesUK.clear();
	}

	// construct boost::shared_ptr<YoYInflationIndex> yoyEU, yoyUK
	// yoyEU.linkTo(boost::shared_ptr<InterpolatedYoYInflationCurve<Linear> > pYTSEU)
	// construct boost::shared_ptr<InterpolatedZeroCurve<Cubic> >
	// construct inflation cap/floor price surfaces
	void setup() {

		// make sure of the evaluation date
		Date eval = Date(Day(23), Month(11), Year(2007));
		Settings::instance().evaluationDate() = eval;

		yoyIndexUK = boost::shared_ptr<YoYInflationIndex>(new YYUKRPIr(true, yoyUK));
		yoyIndexEU = boost::shared_ptr<YoYInflationIndex>(new YYEUHICPr(true, yoyEU));

		// nominal yield curve (interpolated; times assume year parts have 365 days)
		Real timesEUR[] = {0.0109589, 0.0684932, 0.263014, 0.317808, 0.567123, 0.816438,
			   1.06575, 1.31507, 1.56438, 2.0137, 3.01918, 4.01644,
			   5.01644, 6.01644, 7.01644, 8.01644, 9.02192, 10.0192,
			   12.0192, 15.0247, 20.0301, 25.0356, 30.0329, 40.0384,
			   50.0466};
		Real ratesEUR[] = {0.0415600, 0.0426840, 0.0470980, 0.0458506, 0.0449550, 0.0439784,
			   0.0431887, 0.0426604, 0.0422925, 0.0424591, 0.0421477, 0.0421853,
			   0.0424016, 0.0426969, 0.0430804, 0.0435011, 0.0439368, 0.0443825,
			   0.0452589, 0.0463389, 0.0472636, 0.0473401, 0.0470629, 0.0461092,
			   0.0450794};

		Real timesGBP[] = {0.008219178, 0.010958904, 0.01369863,  0.019178082,  0.073972603,
			   0.323287671, 0.57260274,  0.821917808, 1.071232877,  1.320547945,
			   1.506849315, 2.002739726, 3.002739726, 4.002739726,  5.005479452,
			   6.010958904, 7.008219178, 8.005479452, 9.008219178, 10.00821918,
			   12.01369863, 15.0109589,  20.01369863, 25.01917808,  30.02191781,
			   40.03287671, 50.03561644, 60.04109589, 70.04931507};
		Real ratesGBP[] = {0.0577363, 0.0582314, 0.0585265, 0.0587165, 0.0596598,
			   0.0612506, 0.0589676, 0.0570512, 0.0556147, 0.0546082,
			   0.0549492, 0.053801, 0.0529333, 0.0524068, 0.0519712,
			   0.0516615, 0.0513711, 0.0510433, 0.0507974, 0.0504833,
			   0.0498998, 0.0490464, 0.04768, 0.0464862, 0.045452,
			   0.0437699, 0.0425311, 0.0420073, 0.041151};

		vector <Real> r;
		vector <Date> d;
		Size nTimesEUR = LENGTH(timesEUR);
		Size nTimesGBP = LENGTH(timesGBP);
		for (Size i = 0; i < nTimesEUR; i++) {
			r.push_back(ratesEUR[i]);
			Size ys = (Size)floor(timesEUR[i]);
			Size ds = (Size)((timesEUR[i]-(Real)ys)*365);
			Date dd = eval + Period(ys,Years) + Period(ds,Days);
			d.push_back( dd ); 
		}

		boost::shared_ptr<InterpolatedZeroCurve<Cubic> >
			euriborTS(new InterpolatedZeroCurve<Cubic>(d, r, Actual365Fixed()));
		Handle<YieldTermStructure> nominalHeur(euriborTS, false);
		nominalEUR = nominalHeur;   // copy to global

		d.clear();
		r.clear();
		for (Size i = 0; i < nTimesGBP; i++) {
			r.push_back(ratesGBP[i]);
			Size ys = (Size)floor(timesGBP[i]);
			Size ds = (Size)((timesGBP[i]-(Real)ys)*365);
			Date dd = eval + Period(ys,Years) + Period(ds,Days);
			d.push_back( dd );
		}

		boost::shared_ptr<InterpolatedZeroCurve<Cubic> >
			gbpLiborTS(new InterpolatedZeroCurve<Cubic>(d, r, Actual365Fixed()));
		Handle<YieldTermStructure> nominalHgbp(gbpLiborTS, false);
		nominalGBP = nominalHgbp;   // copy to global

		// times = years - lag, where the lag is 2 months or 2/12
		// because this data is derived from cap/floor data that
		// is based on a 2 month lag.

		// note that these are NOT swap rates
		// also not that the first value MUST be in the base period
		// i.e. the first rate is for a negative time
		Real yoyEUrates[] = {0.0237951,
			 0.0238749, 0.0240334, 0.0241934, 0.0243567, 0.0245323,
			 0.0247213, 0.0249348, 0.0251768, 0.0254337, 0.0257258,
			 0.0260217, 0.0263006, 0.0265538, 0.0267803, 0.0269378,
			 0.0270608, 0.0271363, 0.0272, 0.0272512, 0.0272927,
			 0.027317, 0.0273615, 0.0273811, 0.0274063, 0.0274307,
			 0.0274625, 0.027527, 0.0275952, 0.0276734, 0.027794};

		d.clear();
		r.clear();
		Date baseDate = TARGET().advance(eval, -2, Months, ModifiedFollowing);
		for (Size i = 0; i < LENGTH(yoyEUrates); i++) {
			Date dd = TARGET().advance(baseDate, i, Years, ModifiedFollowing);
			d.push_back(dd);
			r.push_back(yoyEUrates[i]);
		}

		bool indexIsInterpolated = true;    // actually false for UKRPI but smooth surfaces are
											// better for finding intersections etc

		boost::shared_ptr<InterpolatedYoYInflationCurve<Linear> >
			pYTSEU( new InterpolatedYoYInflationCurve<Linear>(
					eval, TARGET(), Actual365Fixed(), Period(2,Months), Monthly,
					indexIsInterpolated, nominalGBP, d, r) );
		yoyEU.linkTo(pYTSEU);

		// price data
		const Size ncStrikesEU = 6;
		const Size nfStrikesEU = 6;
		const Size ncfMaturitiesEU = 7;
		Real capStrikesEU[ncStrikesEU] = {0.02, 0.025, 0.03, 0.035, 0.04, 0.05};
		Period capMaturitiesEU[ncfMaturitiesEU] = {3*Years, 5*Years, 7*Years,
			10*Years, 15*Years, 20*Years, 30*Years};
		Real capPricesEU[ncStrikesEU][ncfMaturitiesEU] =
			{{116.225, 204.945, 296.285, 434.29, 654.47, 844.775, 1132.33},
				{34.305, 71.575, 114.1, 184.33, 307.595, 421.395, 602.35},
				{6.37, 19.085, 35.635, 66.42, 127.69, 189.685, 296.195},
				{1.325, 5.745, 12.585, 26.945, 58.95, 94.08, 158.985},
				{0.501, 2.37, 5.38, 13.065, 31.91, 53.95, 96.97},
				{0.501, 0.695, 1.47, 4.415, 12.86, 23.75, 46.7}};

		Real floorStrikesEU[nfStrikesEU] = {-0.01, 0.00, 0.005, 0.01, 0.015, 0.02};
		Real floorPricesEU[nfStrikesEU][ncfMaturitiesEU] =
			{{0.501, 0.851, 2.44, 6.645, 16.23, 26.85, 46.365},
				{0.501, 2.236, 5.555, 13.075, 28.46, 44.525, 73.08},
				{1.025, 3.935, 9.095, 19.64, 39.93, 60.375, 96.02},
				{2.465, 7.885, 16.155, 31.6, 59.34, 86.21, 132.045},
				{6.9, 17.92, 32.085, 56.08, 95.95, 132.85, 194.18},
				{23.52, 47.625, 74.085, 114.355, 175.72, 229.565, 316.285}};

		// now load the data into vector and Matrix classes
		cStrikesEU.clear();
		fStrikesEU.clear();
		cfMaturitiesEU.clear();
		for(Size i = 0; i < ncStrikesEU; i++) cStrikesEU.push_back(capStrikesEU[i]);
		for(Size i = 0; i < nfStrikesEU; i++) fStrikesEU.push_back(floorStrikesEU[i]);
		for(Size i = 0; i < ncfMaturitiesEU; i++) cfMaturitiesEU.push_back(capMaturitiesEU[i]);
		boost::shared_ptr<Matrix> tcPriceEU(new Matrix(ncStrikesEU, ncfMaturitiesEU));
		boost::shared_ptr<Matrix> tfPriceEU(new Matrix(nfStrikesEU, ncfMaturitiesEU));
		for(Size i = 0; i < ncStrikesEU; i++) {
			for(Size j = 0; j < ncfMaturitiesEU; j++) {
				(*tcPriceEU)[i][j] = capPricesEU[i][j];
			}
		}
		for(Size i = 0; i < nfStrikesEU; i++) {
			for(Size j = 0; j < ncfMaturitiesEU; j++) {
				(*tfPriceEU)[i][j] = floorPricesEU[i][j];
			}
		}
		cPriceEU = tcPriceEU;   // copy to global
		fPriceEU = tfPriceEU;
	}

	// Construct boost::shared_ptr<InterpolatedYoYCapFloorTermPriceSurface<Bicubic,Cubic> > priceSurfEU = cfEUprices
	void setupPriceSurface() {

		// construct:
		//  calendar, business day convention, and day counter are
		//  taken from the nominal base given the reference date for
		//  the inflation options (generally 2 or 3 months before
		//  nominal reference date)
		Natural fixingDays = 0;
		Size lag = 3;// must be 3 because we use an interpolated index (EU)
		Period yyLag = Period(lag,Months);
		Rate baseRate = 1; // not really used
		DayCounter dc = Actual365Fixed();
		TARGET cal;
		BusinessDayConvention bdc = ModifiedFollowing;
		boost::shared_ptr<QuantLib::YieldTermStructure> pn =
			nominalEUR.currentLink(); // dereference Handle<YieldTermStructure>
		Handle<QuantLib::YieldTermStructure> n(pn,false);
		boost::shared_ptr<InterpolatedYoYCapFloorTermPriceSurface<Bicubic,Cubic> >
		cfEUprices(new InterpolatedYoYCapFloorTermPriceSurface<Bicubic,Cubic>(
									   fixingDays,
									   yyLag, yoyIndexEU, baseRate,
									   n, dc,
									   cal,    bdc,
									   cStrikesEU, fStrikesEU, cfMaturitiesEU,
									   (*cPriceEU), (*fPriceEU)));
		/*
		template<class I2D, class I1D>
		InterpolatedYoYCapFloorTermPriceSurface<I2D,I1D>::
		InterpolatedYoYCapFloorTermPriceSurface(
											Natural fixingDays,
											const Period &yyLag,
											const boost::shared_ptr<YoYInflationIndex>& yii,
											Rate baseRate,
											const Handle<YieldTermStructure> &nominal,
											const DayCounter &dc,
											const Calendar &cal,
											const BusinessDayConvention &bdc,
											const std::vector<Rate> &cStrikes,
											const std::vector<Rate> &fStrikes,
											const std::vector<Period> &cfMaturities,
											const Matrix &cPrice,
											const Matrix &fPrice,
											const I2D &interpolator2d,
											const I1D &interpolator1d)
			: YoYCapFloorTermPriceSurface(fixingDays, yyLag, yii,
											baseRate, nominal, dc, cal, bdc,
											cStrikes, fStrikes, cfMaturities,
											cPrice, fPrice),
											interpolator2d_(interpolator2d), interpolator1d_(interpolator1d) {
			performCalculations();
		}*/
		priceSurfEU = cfEUprices;
	}

}

namespace {
	struct Datum {
		Date date;
		Rate rate;
	};

	template <class T, class U, class I>
	std::vector<boost::shared_ptr<BootstrapHelper<T> > > makeHelpers(
		Datum iiData[], Size N,
		const boost::shared_ptr<I> &ii, const Period &observationLag,
		const Calendar &calendar,
		const BusinessDayConvention &bdc,
		const DayCounter &dc) {

			std::vector<boost::shared_ptr<BootstrapHelper<T> > > instruments;
			for (Size i=0; i<N; i++) {
				Date maturity = iiData[i].date;
				Handle<Quote> quote(boost::shared_ptr<Quote>(
					new SimpleQuote(iiData[i].rate/100.0)));
				boost::shared_ptr<BootstrapHelper<T> > anInstrument(new U(
					quote, observationLag, maturity,
					calendar, bdc, dc, ii));
				instruments.push_back(anInstrument);
			}

			return instruments;
	}


	struct CommonVars {
		// common data

		Size length;
		Date startDate;
		Real volatility;

		Frequency frequency;
		std::vector<Real> nominals;
		Calendar calendar;
		BusinessDayConvention convention;
		Natural fixingDays;
		Date evaluationDate;
		Natural settlementDays;
		Date settlement;
		Period observationLag, contractObservationLag;
		CPI::InterpolationType contractObservationInterpolation;
		DayCounter dcZCIIS,dcNominal;
		std::vector<Date> zciisD;
		std::vector<Rate> zciisR;
		boost::shared_ptr<UKRPI> ii;
		Size zciisDataLength;

		RelinkableHandle<YieldTermStructure> nominalTS;
		boost::shared_ptr<ZeroInflationTermStructure> cpiTS;

		// cleanup

		SavedSettings backup;

		// setup: construct <ZeroInflationTermStructure> hcpi
		// hcpi.linkTo(boost::shared_ptr<PiecewiseZeroInflationCurve<Linear> > pCPIts)
		CommonVars() {

			// option variables
			nominals = std::vector<Real>(1,1000000);  // 1M
			frequency = Annual;
			// usual setup
			volatility = 0.01;
			length = 7;
			calendar = UnitedKingdom();
			convention = ModifiedFollowing;
			Date today(25, November, 2009);
			evaluationDate = calendar.adjust(today);
			Settings::instance().evaluationDate() = evaluationDate;
			settlementDays = 0;
			fixingDays = 0;
			settlement = calendar.advance(today,settlementDays,Days);
			startDate = settlement;
			dcZCIIS = ActualActual();
			dcNominal = ActualActual();

			// uk rpi index
			//      fixing data
			Date from(20, July, 2007);
			//Date from(20, July, 2008);
			Date to(20, November, 2009);
			Schedule rpiSchedule = MakeSchedule().from(from).to(to)
				.withTenor(1*Months)
				.withCalendar(UnitedKingdom())
				.withConvention(ModifiedFollowing);
			Real fixData[] = {
				206.1, 207.3, 208.0, 208.9, 209.7, 210.9,
				209.8, 211.4, 212.1, 214.0, 215.1, 216.8,
				216.5, 217.2, 218.4, 217.7, 216,
				212.9, 210.1, 211.4, 211.3, 211.5,
				212.8, 213.4, 213.4, 213.4, 214.4,
				-999.0, -999.0 };

			// link from cpi index to cpi TS
			RelinkableHandle<ZeroInflationTermStructure> hcpi;
			bool interp = false;// this MUST be false because the observation lag is only 2 months
			// for ZCIIS; but not for contract if the contract uses a bigger lag.
			ii = boost::shared_ptr<UKRPI>(new UKRPI(interp, hcpi));
			for (Size i=0; i<rpiSchedule.size();i++) {
				ii->addFixing(rpiSchedule[i], fixData[i], true);// force overwrite in case multiple use
			};


			Datum nominalData[] = {
				{ Date(26, November, 2009), 0.475 },
				{ Date(2, December, 2009), 0.47498 },
				{ Date(29, December, 2009), 0.49988 },
				{ Date(25, February, 2010), 0.59955 },
				{ Date(18, March, 2010), 0.65361 },
				{ Date(25, May, 2010), 0.82830 },
				//  { Date(17, June, 2010), 0.7 },  // can't boostrap with this data point
				{ Date(16, September, 2010), 0.78960 },
				{ Date(16, December, 2010), 0.93762 },
				{ Date(17, March, 2011), 1.12037 },
				{ Date(16, June, 2011), 1.31308 },
				{ Date(22, September, 2011),1.52011 },
				{ Date(25, November, 2011), 1.78399 },
				{ Date(26, November, 2012), 2.41170 },
				{ Date(25, November, 2013), 2.83935 },
				{ Date(25, November, 2014), 3.12888 },
				{ Date(25, November, 2015), 3.34298 },
				{ Date(25, November, 2016), 3.50632 },
				{ Date(27, November, 2017), 3.63666 },
				{ Date(26, November, 2018), 3.74723 },
				{ Date(25, November, 2019), 3.83988 },
				{ Date(25, November, 2021), 4.00508 },
				{ Date(25, November, 2024), 4.16042 },
				{ Date(26, November, 2029), 4.15577 },
				{ Date(27, November, 2034), 4.04933 },
				{ Date(25, November, 2039), 3.95217 },
				{ Date(25, November, 2049), 3.80932 },
				{ Date(25, November, 2059), 3.80849 },
				{ Date(25, November, 2069), 3.72677 },
				{ Date(27, November, 2079), 3.63082 }
			};
			const Size nominalDataLength = 30-1;

			std::vector<Date> nomD;
			std::vector<Rate> nomR;
			for (Size i = 0; i < nominalDataLength; i++) {
				nomD.push_back(nominalData[i].date);
				nomR.push_back(nominalData[i].rate/100.0);
			}
			boost::shared_ptr<YieldTermStructure> nominal =
				boost::shared_ptr<InterpolatedZeroCurve<Linear>
				>(new InterpolatedZeroCurve<Linear>(nomD,nomR,dcNominal));


			nominalTS.linkTo(nominal);

			// now build the zero inflation curve
			observationLag = Period(2,Months);
			contractObservationLag = Period(3,Months);
			contractObservationInterpolation = CPI::Flat;

			Datum zciisData[] = {
				{ Date(25, November, 2010), 3.0495 },
				{ Date(25, November, 2011), 2.93 },
				{ Date(26, November, 2012), 2.9795 },
				{ Date(25, November, 2013), 3.029 },
				{ Date(25, November, 2014), 3.1425 },
				{ Date(25, November, 2015), 3.211 },
				{ Date(25, November, 2016), 3.2675 },
				{ Date(25, November, 2017), 3.3625 },
				{ Date(25, November, 2018), 3.405 },
				{ Date(25, November, 2019), 3.48 },
				{ Date(25, November, 2021), 3.576 },
				{ Date(25, November, 2024), 3.649 },
				{ Date(26, November, 2029), 3.751 },
				{ Date(27, November, 2034), 3.77225 },
				{ Date(25, November, 2039), 3.77 },
				{ Date(25, November, 2049), 3.734 },
				{ Date(25, November, 2059), 3.714 },
			};
			zciisDataLength = 17;
			for (Size i = 0; i < zciisDataLength; i++) {
				zciisD.push_back(zciisData[i].date);
				zciisR.push_back(zciisData[i].rate);
			}

			// now build the helpers ...
			std::vector<boost::shared_ptr<BootstrapHelper<ZeroInflationTermStructure> > > helpers =
				makeHelpers<ZeroInflationTermStructure,ZeroCouponInflationSwapHelper,
				ZeroInflationIndex>(zciisData, zciisDataLength, ii,
				observationLag,
				calendar, convention, dcZCIIS);

			// we can use historical or first ZCIIS for this
			// we know historical is WAY off market-implied, so use market implied flat.
			Rate baseZeroRate = zciisData[0].rate/100.0;
			boost::shared_ptr<PiecewiseZeroInflationCurve<Linear> > pCPIts(
				new PiecewiseZeroInflationCurve<Linear>(
				evaluationDate, calendar, dcZCIIS, observationLag,
				ii->frequency(),ii->interpolated(), baseZeroRate,
				Handle<YieldTermStructure>(nominalTS), helpers));
			pCPIts->recalculate();
			cpiTS = boost::dynamic_pointer_cast<ZeroInflationTermStructure>(pCPIts);


			// make sure that the index has the latest zero inflation term structure
			hcpi.linkTo(pCPIts);
		}


	};

	bool checkAbsError(Real x1, Real x2, Real tolerance){
		return std::fabs(x1 - x2) < tolerance;
	}
}

//***************************************************************************


void YYIISTest::testYYIIS() {

	BOOST_MESSAGE("YYIIS test");

	CommonVars common;
	
	YearOnYearInflationSwap::Type yyiistype = YearOnYearInflationSwap::Payer;
	Real  nominal = 1000000.0;
	Rate fixedRate = 0.0;
	Rate spread = 0.03714;
	DayCounter fixedDayCount = ActualActual();
	DayCounter yoyDayCount = ActualActual();
	Date from(20, July, 2007);
	Date to(20, November, 2009);
	Period observationLag(2,Months);
	Schedule fixedSchedule = MakeSchedule().from(from).to(to)
		.withTenor(1*Months)
		.withCalendar(UnitedKingdom())
		.withConvention(ModifiedFollowing);
	Schedule yoySchedule = MakeSchedule().from(from).to(to)
		.withTenor(1*Months)
		.withCalendar(UnitedKingdom())
		.withConvention(ModifiedFollowing);
	Calendar paymentCalendar = UnitedKingdom();
	boost::shared_ptr<YoYInflationIndex> yoyIndex = yoyIndexUK;
	BusinessDayConvention paymentConvention = ModifiedFollowing;

	
	YearOnYearInflationSwap yyiis(
		yyiistype,
		nominal,
		fixedSchedule,
		fixedRate,
		fixedDayCount,
		yoySchedule,
		yoyIndex,
		observationLag,
		spread,
		yoyDayCount,
		paymentCalendar,
		paymentConvention	
		);

	boost::shared_ptr<DiscountingSwapEngine>
		yyiisdse(new DiscountingSwapEngine(common.nominalTS));

	yyiis.setPricingEngine(yyiisdse);
	yyiis.recalculate();

	QL_REQUIRE(fabs(yyiis.NPV())<1e-3,"yyiis does not reprice to zero");

	std::cout << "YYIIS yoy leg NPV  : " << yyiis.yoyLegNPV() << std::endl;;
	std::cout << "YYIIS fixed leg NPV: " << yyiis.fixedLegNPV() << std::endl;
	std::cout << "YYIIS NPV: " << yyiis.NPV() << std::endl;
	std::cout << yyiis.fairRate() << std::endl;

	/*
	YearOnYearInflationSwap(
		Type type,
		Real nominal,
		const Schedule& fixedSchedule,
		Rate fixedRate,
		const DayCounter& fixedDayCount,
		const Schedule& yoySchedule,
		const boost::shared_ptr<YoYInflationIndex>& yoyIndex,
		const Period& observationLag,
		Spread spread,
		const DayCounter& yoyDayCount,
		const Calendar& paymentCalendar,
		BusinessDayConvention paymentConvention)

	ZeroCouponInflationSwap(Type type,
		Real nominal,
		const Date& startDate,   // start date of contract (only)
		const Date& maturity,    // this is pre-adjustment!
		const Calendar& fixCalendar,
		BusinessDayConvention fixConvention,
		const DayCounter& dayCounter,
		Rate fixedRate,
		const boost::shared_ptr<ZeroInflationIndex> &infIndex,
		const Period& observationLag,
		bool adjustInfObsDates = false,
		Calendar infCalendar = Calendar(),
		BusinessDayConvention infConvention = BusinessDayConvention());
		*/

}