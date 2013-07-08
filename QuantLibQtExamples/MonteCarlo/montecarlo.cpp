/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

//#include "../customutilities.hpp"
//#include <ql/quantlib.hpp>
//#include <boost/timer.hpp>
//#include <boost/shared_ptr.hpp>
//#include <iostream>
//#include <iomanip>
#include "../customutilities.hpp"
#include <ql/quantlib.hpp>
#include <boost/timer.hpp>
#include <boost/shared_ptr.hpp>
#include <iostream>
#include <iomanip>

using namespace QuantLib;

namespace NamespaceInflationVolTest {

    class InflationVolTest {
    public:
        void testYoYPriceSurfaceToATM();
        void testYoYPriceSurfaceToVol();
    };

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


    void InflationVolTest::testYoYPriceSurfaceToVol() {
        BOOST_MESSAGE("Testing conversion from YoY price surface to YoY volatility surface...");
        //std::cout << "Testing conversion from YoY price surface to YoY volatility surface..." << std::endl;

        SavedSettings backup;

        setup();

        // first get the price surface set up
        setupPriceSurface();

        // caplet pricer, recall that setCapletVolatility(Handle<YoYOptionletVolatilitySurface>)
        // exists ... we'll use it with the -Curve variant of the surface
        // test UNIT DISPLACED pricer
        boost::shared_ptr<YoYOptionletVolatilitySurface> pVS;
        Handle<YoYOptionletVolatilitySurface> hVS(pVS, false); // pVS does NOT own whatever it points to later, hence the handle does not either
        boost::shared_ptr<YoYInflationUnitDisplacedBlackCapFloorEngine>
            yoyPricerUD(new YoYInflationUnitDisplacedBlackCapFloorEngine(yoyIndexEU,hVS)); //hVS
        // N.B. the vol gets set in the stripper ... else no point!

        // cap stripper
        boost::shared_ptr<YoYOptionletStripper> yoyOptionletStripper(
                                 new InterpolatedYoYOptionletStripper<Linear>() );

        // now set up all the variables for the stripping
        Natural settlementDays = 0;
        TARGET cal;
        BusinessDayConvention bdc = ModifiedFollowing;
        DayCounter dc = Actual365Fixed();

        boost::shared_ptr<YoYCapFloorTermPriceSurface> capFloorPrices = priceSurfEU;
        Period lag = priceSurfEU->observationLag();

        Real slope = -0.5; //when you have bad data, i.e. very low/constant
        //prices for short dated extreem strikes
        //then you cannot assume constant caplet vol
        //(else arbitrage)
        // N.B. if this is too extreme then can't
        // get a no-arbitrage solution anyway
        // the way the slope is used means that the slope is
        // proportional to the level so higher slopes at
        // the edges when things are more volatile

        // Actually it doesn't matter what the interpolation is because we only
        // intend to use the K values that correspond to quotes ... for model fitting.
        boost::shared_ptr<KInterpolatedYoYOptionletVolatilitySurface<Linear> > yoySurf(new
                        KInterpolatedYoYOptionletVolatilitySurface<Linear>(settlementDays,
                    cal, bdc, dc, lag, capFloorPrices, yoyPricerUD, yoyOptionletStripper,
                                                                  slope) );

        // now use it for something ... like stating what the T=const lines look like
        const Real volATyear1[] = {
              0.0128, 0.0093, 0.0083, 0.0073, 0.0064,
              0.0058, 0.0042, 0.0046, 0.0053, 0.0064,
              0.0098
        };
        const Real volATyear3[] = {
              0.0079, 0.0058, 0.0051, 0.0045, 0.0039,
              0.0035, 0.0026, 0.0028, 0.0033, 0.0039,
              0.0060
        };

        Date d = yoySurf->baseDate() + Period(1,Years);
        pair<vector<Rate>, vector<Volatility> > someSlice;
        someSlice = yoySurf->Dslice(d);

        Size n = someSlice.first.size();
        Real eps = 0.0001;
        std::cout << "\nYoY inflation volatility slice in year 1" << std::endl;
        for(Size i = 0; i < n; i++){
            std::cout << "   " << volATyear1[i] << "  " << someSlice.second[i] << std::endl;
            QL_REQUIRE( fabs(someSlice.second[i] - volATyear1[i]) < eps,
                       " could not recover 1yr vol: " << someSlice.second[i]
                       << " vs " << volATyear1[i] );
        }

        d = yoySurf->baseDate() + Period(3,Years);
        pair<vector<Rate>, vector<Volatility> >
            someOtherSlice = yoySurf->Dslice(d);
        n = someOtherSlice.first.size();
        std::cout << "\nYoY inflation volatility slice in year 3" << std::endl;
        for(Size i = 0; i < n; i++){
            std::cout << "   " << volATyear3[i] << "  " << someOtherSlice.second[i] << std::endl;
            QL_REQUIRE(fabs(someOtherSlice.second[i]-volATyear3[i]) < eps,
                            "could not recover 3yr vol: "
                            << someOtherSlice.second[i]<< " vs " << volATyear3[i] );
        }

        std::cout << "\nYoY inflation volatility surface" << std::endl;
        for(Size infYear = 1; infYear < 5; infYear++){
            for(Size strikeRate = 0; strikeRate < n; strikeRate++){
                std::cout << yoySurf->Dslice(yoySurf->baseDate() + Period(infYear,Years)).second[strikeRate] << " ";
            }
            std::cout << std::endl;
        }

        reset();
    }




    void InflationVolTest::testYoYPriceSurfaceToATM() {
        BOOST_MESSAGE("Testing conversion from YoY cap-floor surface to YoY inflation term structure...");
        //std::cout << "Testing conversion from YoY cap-floor surface to YoY inflation term structure..." << std::endl;

        SavedSettings backup;

        setup();

        setupPriceSurface();

        pair<vector<Time>, vector<Rate> > yyATMt = priceSurfEU->atmYoYSwapTimeRates();
        pair<vector<Date>, vector<Rate> > yyATMd = priceSurfEU->atmYoYSwapDateRates();

        // Real dy = (Real)lag / 12.0;
        const Real crv[] = {0.024586, 0.0247575, 0.0249396, 0.0252596,
                              0.0258498, 0.0262883, 0.0267915};
        const Real swaps[] = {0.024586, 0.0247575, 0.0249396, 0.0252596,
                              0.0258498, 0.0262883, 0.0267915};
        const Real ayoy[] = {0.0247659, 0.0251437, 0.0255945, 0.0265234,
                               0.0280457, 0.0285534, 0.0295884};
        Real eps = 2e-5;
        for(Size i = 0; i < yyATMt.first.size(); i++) {
            QL_REQUIRE(fabs( yyATMt.second[i] - crv[i] ) < eps,
                       "could not recover cached yoy swap curve "
                       << yyATMt.second[i]<< " vs " << crv[i]);
        }

        for(Size i = 0; i < yyATMd.first.size(); i++) {
            // atmYoYSwapRate(yyATMd.first[i])
            QL_REQUIRE(fabs( priceSurfEU->atmYoYSwapRate(yyATMd.first[i])  - swaps[i] ) < eps,
                       "could not recover yoy swap curve "
                       << priceSurfEU->atmYoYSwapRate(yyATMd.first[i]) << " vs " << swaps[i]);
        }
        for(Size i = 0; i < yyATMd.first.size(); i++) {
            // atmYoYRate(yyATMd.first[i])
            QL_REQUIRE(fabs( priceSurfEU->atmYoYRate(yyATMd.first[i])  - ayoy[i] ) < eps,
                       " could not recover cached yoy curve "
                       << priceSurfEU->atmYoYRate(yyATMd.first[i]) << " vs " << ayoy[i]
                       <<" at "<<yyATMd.first[i]);
        }
        reset();
    }
}


namespace NamespaceInflationCPICapFloorTest {

    class InflationCPICapFloorTest {
    public:
        //! tests CPI price surface reproduction
        void cpicapfloorpricesurface();
        //! tests interpolation pricer
        void cpicapfloorpricer();
    };

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
        Rate baseZeroRate;
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
        RelinkableHandle<ZeroInflationIndex> hii;
        Size zciisDataLength;

        RelinkableHandle<YieldTermStructure> nominalUK;
        RelinkableHandle<ZeroInflationTermStructure> cpiUK;
        RelinkableHandle<ZeroInflationTermStructure> hcpi;

        std::vector<Rate> cStrikesUK;
        std::vector<Rate> fStrikesUK;
        std::vector<Period> cfMaturitiesUK;
        boost::shared_ptr<Matrix> cPriceUK;
        boost::shared_ptr<Matrix> fPriceUK;

        boost::shared_ptr<CPICapFloorTermPriceSurface> cpiCFsurfUK;

        // cleanup

        SavedSettings backup;

        // setup
        CommonVars() {
            std::cout <<"CommonVars" << std::endl;
            // option variables
            nominals = std::vector<Real>(1,1000000);  // 1M
            frequency = Annual;
            // usual setup
            volatility = 0.01;
            length = 7;
            calendar = UnitedKingdom();
            convention = ModifiedFollowing;
            Date today(1, June, 2010);
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
            Date from(1, July, 2007);
            Date to(1, June, 2010);
            Schedule rpiSchedule = MakeSchedule().from(from).to(to)
            .withTenor(1*Months)
            .withCalendar(UnitedKingdom())
            .withConvention(ModifiedFollowing);
            Real fixData[] = {
                206.1, 207.3, 208.0, 208.9, 209.7, 210.9,
                209.8, 211.4, 212.1, 214.0, 215.1, 216.8,   //  2008
                216.5, 217.2, 218.4, 217.7, 216.0, 212.9,
                210.1, 211.4, 211.3, 211.5, 212.8, 213.4,   //  2009
                213.4, 214.4, 215.3, 216.0, 216.6, 218.0,
                217.9, 219.2, 220.7, 222.8, -999, -999,     //  2010
                -999};

            // link from cpi index to cpi TS
            bool interp = false;// this MUST be false because the observation lag is only 2 months
                                // for ZCIIS; but not for contract if the contract uses a bigger lag.
            ii = boost::shared_ptr<UKRPI>(new UKRPI(interp, hcpi));
            for (Size i=0; i<rpiSchedule.size();i++) {
                ii->addFixing(rpiSchedule[i], fixData[i], true);// force overwrite in case multiple use
            };


            Datum nominalData[] = {
                { Date( 2, June, 2010), 0.499997 },
                { Date( 3, June, 2010), 0.524992 },
                { Date( 8, June, 2010), 0.524974 },
                { Date( 15, June, 2010), 0.549942 },
                { Date( 22, June, 2010), 0.549913 },
                { Date( 1, July, 2010), 0.574864 },
                { Date( 2, August, 2010), 0.624668 },
                { Date( 1, September, 2010), 0.724338 },
                { Date( 16, September, 2010), 0.769461 },
                { Date( 1, December, 2010), 0.997501 },
                //{ Date( 16, December, 2010), 0.838164 },
                { Date( 17, March, 2011), 0.916996 },
                { Date( 16, June, 2011), 0.984339 },
                { Date( 22, September, 2011), 1.06085 },
                { Date( 22, December, 2011), 1.141788 },
                { Date( 1, June, 2012), 1.504426 },
                { Date( 3, June, 2013), 1.92064 },
                { Date( 2, June, 2014), 2.290824 },
                { Date( 1, June, 2015), 2.614394 },
                { Date( 1, June, 2016), 2.887445 },
                { Date( 1, June, 2017), 3.122128 },
                { Date( 1, June, 2018), 3.322511 },
                { Date( 3, June, 2019), 3.483997 },
                { Date( 1, June, 2020), 3.616896 },
                { Date( 1, June, 2022), 3.8281 },
                { Date( 2, June, 2025), 4.0341 },
                { Date( 3, June, 2030), 4.070854 },
                { Date( 1, June, 2035), 4.023202 },
                { Date( 1, June, 2040), 3.954748 },
                { Date( 1, June, 2050), 3.870953 },
                { Date( 1, June, 2060), 3.85298 },
                { Date( 2, June, 2070), 3.757542 },
                { Date( 3, June, 2080), 3.651379 }
            };
            const Size nominalDataLength = 33-1;

            std::vector<Date> nomD;
            std::vector<Rate> nomR;
            for (Size i = 0; i < nominalDataLength; i++) {
                nomD.push_back(nominalData[i].date);
                nomR.push_back(nominalData[i].rate/100.0);
            }
            boost::shared_ptr<YieldTermStructure>   nominalTS
            =   boost::shared_ptr<InterpolatedZeroCurve<Linear>
            >(new InterpolatedZeroCurve<Linear>(nomD,nomR,dcNominal));


            nominalUK.linkTo(nominalTS);


            // now build the zero inflation curve
            observationLag = Period(2,Months);
            contractObservationLag = Period(3,Months);
            contractObservationInterpolation = CPI::Flat;

            Datum zciisData[] = {
                { Date(1, June, 2011), 3.087 },
                { Date(1, June, 2012), 3.12 },
                { Date(1, June, 2013), 3.059 },
                { Date(1, June, 2014), 3.11 },
                { Date(1, June, 2015), 3.15 },
                { Date(1, June, 2016), 3.207 },
                { Date(1, June, 2017), 3.253 },
                { Date(1, June, 2018), 3.288 },
                { Date(1, June, 2019), 3.314 },
                { Date(1, June, 2020), 3.401 },
                { Date(1, June, 2022), 3.458 },
                { Date(1, June, 2025), 3.52 },
                { Date(1, June, 2030), 3.655 },
                { Date(1, June, 2035), 3.668 },
                { Date(1, June, 2040), 3.695 },
                { Date(1, June, 2050), 3.634 },
                { Date(1, June, 2060), 3.629 },
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
            baseZeroRate = zciisData[0].rate/100.0;
            boost::shared_ptr<PiecewiseZeroInflationCurve<Linear> > pCPIts(
                                new PiecewiseZeroInflationCurve<Linear>(
                                    evaluationDate, calendar, dcZCIIS, observationLag,
                                    ii->frequency(),ii->interpolated(), baseZeroRate,
                                    Handle<YieldTermStructure>(nominalTS), helpers));
            pCPIts->recalculate();
            cpiUK.linkTo(pCPIts);
            hii.linkTo(ii);

            // make sure that the index has the latest zero inflation term structure
            hcpi.linkTo(pCPIts);

            // cpi CF price surf data
            Period cfMat[] = {3*Years, 5*Years, 7*Years, 10*Years, 15*Years, 20*Years, 30*Years};
            Real cStrike[] = {3, 4, 5, 6};
            Real fStrike[] = {-1, 0, 1, 2};
            Size ncStrikes = 4, nfStrikes = 4, ncfMaturities = 7;

            Real cPrice[7][4] = {
                {227.6, 100.27, 38.8, 14.94},
                {345.32, 127.9, 40.59, 14.11},
                {477.95, 170.19, 50.62, 16.88},
                {757.81, 303.95, 107.62, 43.61},
                {1140.73, 481.89, 168.4, 63.65},
                {1537.6, 607.72, 172.27, 54.87},
                {2211.67, 839.24, 184.75, 45.03}};
            Real fPrice[7][4] = {
                {15.62, 28.38, 53.61, 104.6},
                {21.45, 36.73, 66.66, 129.6},
                {24.45, 42.08, 77.04, 152.24},
                {39.25, 63.52, 109.2, 203.44},
                {36.82, 63.62, 116.97, 232.73},
                {39.7, 67.47, 121.79, 238.56},
                {41.48, 73.9, 139.75, 286.75}};

            // now load the data into vector and Matrix classes
            cStrikesUK.clear();
            fStrikesUK.clear();
            cfMaturitiesUK.clear();
            for(Size i = 0; i < ncStrikes; i++) cStrikesUK.push_back(cStrike[i]);
            for(Size i = 0; i < nfStrikes; i++) fStrikesUK.push_back(fStrike[i]);
            for(Size i = 0; i < ncfMaturities; i++) cfMaturitiesUK.push_back(cfMat[i]);
            cPriceUK = boost::shared_ptr<Matrix>(new Matrix(ncStrikes, ncfMaturities));
            fPriceUK = boost::shared_ptr<Matrix>(new Matrix(nfStrikes, ncfMaturities));
            for(Size i = 0; i < ncStrikes; i++) {
                for(Size j = 0; j < ncfMaturities; j++) {
                    (*cPriceUK)[i][j] = cPrice[j][i]/10000.0;
                }
            }
            for(Size i = 0; i < nfStrikes; i++) {
                for(Size j = 0; j < ncfMaturities; j++) {
                    (*fPriceUK)[i][j] = fPrice[j][i]/10000.0;
                }
            }



        }


    };

    bool checkAbsError(Real x1, Real x2, Real tolerance){
        return std::fabs(x1 - x2) < tolerance;
    }


    void InflationCPICapFloorTest::cpicapfloorpricesurface() {

        // check inflation leg vs calculation directly from inflation TS
        CommonVars common;


        Real nominal = 1.0;
        InterpolatedCPICapFloorTermPriceSurface
        <Bilinear> cpiSurf(nominal,
                           common.baseZeroRate,
                           common.observationLag,
                           common.calendar,
                           common.convention,
                           common.dcZCIIS,
                           common.hii,
                           common.nominalUK,
                           common.cStrikesUK,
                           common.fStrikesUK,
                           common.cfMaturitiesUK,
                           *(common.cPriceUK),
                           *(common.fPriceUK));

         // test code - note order of indices
         std::cout << "\nReproducing cpi floor data from cpi surface" << std::endl;
         std::cout << "strike\tmaturity\tfloorprice\tfloorprice" << std::endl;
         for (Size i =0; i<common.fStrikesUK.size(); i++){

             Real qK = common.fStrikesUK[i];
             Size nMat = common.cfMaturitiesUK.size();
             for (Size j=0; j<nMat; j++) {
                 Period t = common.cfMaturitiesUK[j];
                 Real a = (*(common.fPriceUK))[i][j];
                 Real b = cpiSurf.floorPrice(t,qK);

                 std::cout << qK << "\t" << t << "\t" << a << "\t" << b << std::endl;

                 QL_REQUIRE(fabs(a-b)<1e-7,"cannot reproduce cpi floor data from surface: "
                            << a << " vs constructed = " << b);
             }
         }
        std::cout << "\nReproducing cpi cap data from cpi surface" << std::endl;
        std::cout << "strike\tmaturity\tcapprice\tcapprice" << std::endl;
        for (Size i =0; i<common.cStrikesUK.size(); i++){

            Real qK = common.cStrikesUK[i];
            Size nMat = common.cfMaturitiesUK.size();
            for (Size j=0; j<nMat; j++) {
                Period t = common.cfMaturitiesUK[j];
                Real a = (*(common.cPriceUK))[i][j];
                Real b = cpiSurf.capPrice(t,qK);

                std::cout << qK << "\t" << t << "\t" << a << "\t" << b << std::endl;

                QL_REQUIRE(fabs(a-b)<1e-7,"cannot reproduce cpi cap data from surface: "
                           << a << " vs constructed = " << b);
            }
        }

        // remove circular refernce
        common.hcpi.linkTo(boost::shared_ptr<ZeroInflationTermStructure>());
    }


    void InflationCPICapFloorTest::cpicapfloorpricer() {

        CommonVars common;
        Real nominal = 1.0;
        boost::shared_ptr<CPICapFloorTermPriceSurface> cpiCFpriceSurf(new InterpolatedCPICapFloorTermPriceSurface
                                                        <Bilinear>(nominal,
                                                                   common.baseZeroRate,
                                                                   common.observationLag,
                                                                   common.calendar,
                                                                   common.convention,
                                                                   common.dcZCIIS,
                                                                   common.hii,
                                                                   common.nominalUK,
                                                                   common.cStrikesUK,
                                                                   common.fStrikesUK,
                                                                   common.cfMaturitiesUK,
                                                                   *(common.cPriceUK),
                                                                   *(common.fPriceUK)));

        common.cpiCFsurfUK = cpiCFpriceSurf;

        // interpolation pricer first
        // N.B. no new instrument required but we do need a new pricer

        Date startDate = Settings::instance().evaluationDate();
        Date maturity(startDate + Period(3,Years));
        Calendar fixCalendar = UnitedKingdom(), payCalendar = UnitedKingdom();
        BusinessDayConvention fixConvention(Unadjusted), payConvention(ModifiedFollowing);
        Rate strike(0.03);
        Real baseCPI = common.hii->fixing(fixCalendar.adjust(startDate-common.observationLag,fixConvention));
        CPI::InterpolationType observationInterpolation = CPI::AsIndex;
        CPICapFloor aCap(Option::Call,
                         nominal,
                         startDate,   // start date of contract (only)
                         baseCPI,
                         maturity,    // this is pre-adjustment!
                         fixCalendar,
                         fixConvention,
                         payCalendar,
                         payConvention,
                         strike,
                         common.hii,
                         common.observationLag,
                         observationInterpolation);

        Handle<CPICapFloorTermPriceSurface> cpiCFsurfUKh(common.cpiCFsurfUK);
        boost::shared_ptr<PricingEngine>engine(new InterpolatingCPICapFloorEngine(cpiCFsurfUKh));

        aCap.setPricingEngine(engine);

        Date d = common.cpiCFsurfUK->cpiOptionDateFromTenor(Period(3,Years));

        Real cached = cpiCFsurfUKh->capPrice(d, strike);
        QL_REQUIRE(fabs(cached - aCap.NPV())<1e-10,"InterpolatingCPICapFloorEngine does not reproduce cached price: "
                   << cached << " vs " << aCap.NPV());

        std::cout << "CPI cap NPV (calc):   " << aCap.NPV() << std::endl;
        std::cout << "CPI cap NPV (cached): " << cached << std::endl;

        // remove circular refernce
        common.hcpi.linkTo(boost::shared_ptr<ZeroInflationTermStructure>());
    }

}


namespace NamespaceInflationYoYCapFloorTest {

    class InflationYoYCapFloorTest {
    public:
        static void testConsistency();
        static void testParity();
        static void testCachedValue();
    };

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

        Frequency frequency;
        std::vector<Real> nominals;
        Calendar calendar;
        BusinessDayConvention convention;
        Natural fixingDays;
        Date evaluationDate;
        Natural settlementDays;
        Date settlement;
        Period observationLag;
        DayCounter dc;
        boost::shared_ptr<YYUKRPIr> iir;

        RelinkableHandle<YieldTermStructure> nominalTS;
        boost::shared_ptr<YoYInflationTermStructure> yoyTS;
        RelinkableHandle<YoYInflationTermStructure> hy;

        // cleanup

        SavedSettings backup;

        // setup
        CommonVars() {
            // option variables
            nominals = std::vector<Real>(1,1000000);
            frequency = Annual;
            // usual setup
            calendar = UnitedKingdom();
            convention = ModifiedFollowing;
            Date today(13, August, 2007);
            evaluationDate = calendar.adjust(today);
            Settings::instance().evaluationDate() = evaluationDate;
            settlementDays = 0;
            fixingDays = 0;
            settlement = calendar.advance(today,settlementDays,Days);
            dc = Thirty360();

            // yoy index
            //      fixing data
            Date from(1, January, 2005);
            Date to(13, August, 2007);
            Schedule rpiSchedule = MakeSchedule().from(from).to(to)
            .withTenor(1*Months)
            .withCalendar(UnitedKingdom())
            .withConvention(ModifiedFollowing);
            Real fixData[] = { 189.9, 189.9, 189.6, 190.5, 191.6, 192.0,
                192.2, 192.2, 192.6, 193.1, 193.3, 193.6,
                194.1, 193.4, 194.2, 195.0, 196.5, 197.7,
                198.5, 198.5, 199.2, 200.1, 200.4, 201.1,
                202.7, 201.6, 203.1, 204.4, 205.4, 206.2,
                207.3, -999.0, -999 };
            // link from yoy index to yoy TS
            bool interp = false;
            iir = boost::shared_ptr<YYUKRPIr>(new YYUKRPIr(interp, hy));
            for (Size i=0; i<rpiSchedule.size();i++) {
                iir->addFixing(rpiSchedule[i], fixData[i]);
            }

            boost::shared_ptr<YieldTermStructure> nominalFF(
                new FlatForward(evaluationDate, 0.05, ActualActual()));
            nominalTS.linkTo(nominalFF);

            // now build the YoY inflation curve
            Period observationLag = Period(2,Months);

            Datum yyData[] = {
                { Date(13, August, 2008), 2.95 },
                { Date(13, August, 2009), 2.95 },
                { Date(13, August, 2010), 2.93 },
                { Date(15, August, 2011), 2.955 },
                { Date(13, August, 2012), 2.945 },
                { Date(13, August, 2013), 2.985 },
                { Date(13, August, 2014), 3.01 },
                { Date(13, August, 2015), 3.035 },
                { Date(13, August, 2016), 3.055 },  // note that
                { Date(13, August, 2017), 3.075 },  // some dates will be on
                { Date(13, August, 2019), 3.105 },  // holidays but the payment
                { Date(15, August, 2022), 3.135 },  // calendar will roll them
                { Date(13, August, 2027), 3.155 },
                { Date(13, August, 2032), 3.145 },
                { Date(13, August, 2037), 3.145 }
            };

            // now build the helpers ...
            std::vector<boost::shared_ptr<BootstrapHelper<YoYInflationTermStructure> > > helpers =
            makeHelpers<YoYInflationTermStructure,YearOnYearInflationSwapHelper,
            YoYInflationIndex>(yyData, LENGTH(yyData), iir,
                               observationLag,
                               calendar, convention, dc);

            Rate baseYYRate = yyData[0].rate/100.0;
            boost::shared_ptr<PiecewiseYoYInflationCurve<Linear> > pYYTS(
                new PiecewiseYoYInflationCurve<Linear>(
                        evaluationDate, calendar, dc, observationLag,
                        iir->frequency(),iir->interpolated(), baseYYRate,
                        Handle<YieldTermStructure>(nominalTS), helpers));
            pYYTS->recalculate();
            yoyTS = boost::dynamic_pointer_cast<YoYInflationTermStructure>(pYYTS);


            // make sure that the index has the latest yoy term structure
            hy.linkTo(pYYTS);
        }

        // utilities
        Leg makeYoYLeg(const Date& startDate, Integer length) {
            boost::shared_ptr<YoYInflationIndex> ii =
                boost::dynamic_pointer_cast<YoYInflationIndex>(iir);
            Date endDate = calendar.advance(startDate,length*Years,Unadjusted);
            Schedule schedule(startDate, endDate, Period(frequency), calendar,
                              Unadjusted,Unadjusted,// ref periods & acc periods
                              DateGeneration::Forward, false);
            return yoyInflationLeg(schedule, calendar, ii, observationLag)
            .withNotionals(nominals)
            .withPaymentDayCounter(dc)
            .withPaymentAdjustment(convention);
        }


        boost::shared_ptr<PricingEngine> makeEngine(Volatility volatility,
                                                    Size which) {

            boost::shared_ptr<YoYInflationIndex>
            yyii = boost::dynamic_pointer_cast<YoYInflationIndex>(iir);

            Handle<YoYOptionletVolatilitySurface>
                vol(boost::shared_ptr<ConstantYoYOptionletVolatility>(
                    new ConstantYoYOptionletVolatility(volatility,
                                                       settlementDays,
                                                       calendar,
                                                       convention,
                                                       dc,
                                                       observationLag,
                                                       frequency,
                                                       iir->interpolated())));


            switch (which) {
                case 0:
                    return boost::shared_ptr<PricingEngine>(
                            new YoYInflationBlackCapFloorEngine(iir, vol));
                    break;
                case 1:
                    return boost::shared_ptr<PricingEngine>(
                            new YoYInflationUnitDisplacedBlackCapFloorEngine(iir, vol));
                    break;
                case 2:
                    return boost::shared_ptr<PricingEngine>(
                            new YoYInflationBachelierCapFloorEngine(iir, vol));
                    break;
                default:
                    BOOST_FAIL("unknown engine request: which = "<<which
                               <<"should be 0=Black,1=DD,2=Bachelier");
                    break;
            }
            // make compiler happy
            QL_FAIL("never get here - no engine resolution");
        }


        boost::shared_ptr<YoYInflationCapFloor> makeYoYCapFloor(YoYInflationCapFloor::Type type,
                                                 const Leg& leg,
                                                 Rate strike,
                                                 Volatility volatility,
                                                 Size which) {
            boost::shared_ptr<YoYInflationCapFloor> result;
            switch (type) {
                case YoYInflationCapFloor::Cap:
                    result = boost::shared_ptr<YoYInflationCapFloor>(
                        new YoYInflationCap(leg, std::vector<Rate>(1, strike)));
                    break;
                case YoYInflationCapFloor::Floor:
                    result = boost::shared_ptr<YoYInflationCapFloor>(
                        new YoYInflationFloor(leg, std::vector<Rate>(1, strike)));
                    break;
                default:
                    QL_FAIL("unknown YoYInflation cap/floor type");
            }
            result->setPricingEngine(makeEngine(volatility, which));
            return result;
        }

    };

    bool checkAbsError(Real x1, Real x2, Real tolerance){
        return std::fabs(x1 - x2) < tolerance;
    }

    std::string typeToString(YoYInflationCapFloor::Type type) {
        switch (type) {
            case YoYInflationCapFloor::Cap:
                return "YoYInflation cap";
            case YoYInflationCapFloor::Floor:
                return "YoYInflation floor";
            case YoYInflationCapFloor::Collar:
                return "YoYInflation collar";
            default:
                QL_FAIL("unknown YoYInflation cap/floor type");
        }
    }

    void InflationYoYCapFloorTest::testConsistency() {

        BOOST_MESSAGE("Testing consistency between yoy inflation cap, floor and collar...");

        CommonVars vars;

        Integer lengths[] = { 1, 2, 3, 5, 7, 10, 15, 20 };
        Rate cap_rates[] = { 0.01, 0.025, 0.029, 0.03, 0.031, 0.035, 0.07 };
        Rate floor_rates[] = { 0.01, 0.025, 0.029, 0.03, 0.031, 0.035, 0.07 };
        Volatility vols[] = { 0.001, 0.005, 0.010, 0.015, 0.020 };

        for (Size whichPricer = 0; whichPricer < 3; whichPricer++) {
        for (Size i=0; i<LENGTH(lengths); i++) {
            for (Size j=0; j<LENGTH(cap_rates); j++) {
                for (Size k=0; k<LENGTH(floor_rates); k++) {
                    for (Size l=0; l<LENGTH(vols); l++) {

                        Leg leg = vars.makeYoYLeg(vars.evaluationDate,lengths[i]);

                        boost::shared_ptr<YoYInflationCapFloor> cap
                        = vars.makeYoYCapFloor(YoYInflationCapFloor::Cap,
                                               leg, cap_rates[j], vols[l], whichPricer);

                        boost::shared_ptr<YoYInflationCapFloor> floor
                        = vars.makeYoYCapFloor(YoYInflationCapFloor::Floor,
                                               leg, floor_rates[k], vols[l], whichPricer);

                        YoYInflationCollar collar(leg,std::vector<Rate>(1,cap_rates[j]),
                                      std::vector<Rate>(1,floor_rates[k]));
                        collar.setPricingEngine(vars.makeEngine(vols[l], whichPricer));

                        if (std::fabs((cap->NPV()-floor->NPV())-collar.NPV()) > 1e-6) {
                            BOOST_FAIL(
                                       "inconsistency between cap, floor and collar:\n"
                                       << "    length:       " << lengths[i] << " years\n"
                                       << "    volatility:   " << io::volatility(vols[l]) << "\n"
                                       << "    cap value:    " << cap->NPV()
                                       << " at strike: " << io::rate(cap_rates[j]) << "\n"
                                       << "    floor value:  " << floor->NPV()
                                       << " at strike: " << io::rate(floor_rates[k]) << "\n"
                                       << "    collar value: " << collar.NPV());


                            // test re-composition by optionlets, N.B. ONE per year
                            Real capletsNPV = 0.0;
                            std::vector<boost::shared_ptr<YoYInflationCapFloor> > caplets;
                            for (Integer m=0; m<lengths[i]*1; m++) {
                                caplets.push_back(cap->optionlet(m));
                                caplets[m]->setPricingEngine(vars.makeEngine(vols[l], whichPricer));
                                capletsNPV += caplets[m]->NPV();
                            }

                            if (std::fabs(cap->NPV() - capletsNPV) > 1e-6) {
                                BOOST_FAIL(
                                           "sum of caplet NPVs does not equal cap NPV:\n"
                                           << "    length:       " << lengths[i] << " years\n"
                                           << "    volatility:   " << io::volatility(vols[l]) << "\n"
                                           << "    cap value:    " << cap->NPV()
                                           << " at strike: " << io::rate(cap_rates[j]) << "\n"
                                           << "    sum of caplets value:  " << capletsNPV
                                           << " at strike (first): " << io::rate(caplets[0]->capRates()[0]) << "\n"
                                           );
                            }

                            Real floorletsNPV = 0.0;
                            std::vector<boost::shared_ptr<YoYInflationCapFloor> > floorlets;
                            for (Integer m=0; m<lengths[i]*1; m++) {
                                floorlets.push_back(floor->optionlet(m));
                                floorlets[m]->setPricingEngine(vars.makeEngine(vols[l], whichPricer));
                                floorletsNPV += floorlets[m]->NPV();
                            }

                            if (std::fabs(floor->NPV() - floorletsNPV) > 1e-6) {
                                BOOST_FAIL(
                                           "sum of floorlet NPVs does not equal floor NPV:\n"
                                           << "    length:       " << lengths[i] << " years\n"
                                           << "    volatility:   " << io::volatility(vols[l]) << "\n"
                                           << "    cap value:    " << floor->NPV()
                                           << " at strike: " << io::rate(floor_rates[j]) << "\n"
                                           << "    sum of floorlets value:  " << floorletsNPV
                                           << " at strike (first): " << io::rate(floorlets[0]->floorRates()[0]) << "\n"
                                           );
                            }

                            Real collarletsNPV = 0.0;
                            std::vector<boost::shared_ptr<YoYInflationCapFloor> > collarlets;
                            for (Integer m=0; m<lengths[i]*1; m++) {
                                collarlets.push_back(collar.optionlet(m));
                                collarlets[m]->setPricingEngine(vars.makeEngine(vols[l], whichPricer));
                                collarletsNPV += collarlets[m]->NPV();
                            }

                            if (std::fabs(collar.NPV() - collarletsNPV) > 1e-6) {
                                BOOST_FAIL(
                                           "sum of collarlet NPVs does not equal floor NPV:\n"
                                           << "    length:       " << lengths[i] << " years\n"
                                           << "    volatility:   " << io::volatility(vols[l]) << "\n"
                                           << "    cap value:    " << collar.NPV()
                                           << " at strike floor: " << io::rate(floor_rates[j])
                                           << " at strike cap: " << io::rate(cap_rates[j]) << "\n"
                                           << "    sum of collarlets value:  " << collarletsNPV
                                           << " at strike floor (first): " << io::rate(collarlets[0]->floorRates()[0])
                                           << " at strike cap (first): " << io::rate(collarlets[0]->capRates()[0]) << "\n"
                                           );
                            }




                        }
                    }
                }
            }
        }
        } // pricer loop
        // remove circular refernce
        vars.hy.linkTo(boost::shared_ptr<YoYInflationTermStructure>());
    }


    // Test inflation cap/floor parity, i.e. that cap-floor = swap, note that this
    // is different from nominal because in nominal world standard cap/floors do
    // not have the first optionlet.  This is because they set in advance so
    // there is no point.  However, yoy inflation generally sets in arrears,
    // (actually in arrears with a lag of a few months) thus the first optionlet
    // is relevant.  Hence we can do a parity test without a special definition
    // of the YoY cap/floor instrument.
    void InflationYoYCapFloorTest::testParity() {

        BOOST_MESSAGE("Testing yoy inflation cap/floor parity...");

        CommonVars vars;

        Integer lengths[] = { 1, 2, 3, 5, 7, 10, 15, 20 };
        // vol is low ...
        Rate strikes[] = { 0., 0.025, 0.029, 0.03, 0.031, 0.035, 0.07 };
        // yoy inflation vol is generally very low
        Volatility vols[] = { 0.001, 0.005, 0.010, 0.015, 0.020 };

        // cap-floor-swap parity is model-independent
        for (Size whichPricer = 0; whichPricer < 3; whichPricer++) {
            for (Size i=0; i<LENGTH(lengths); i++) {
                for (Size j=0; j<LENGTH(strikes); j++) {
                    for (Size k=0; k<LENGTH(vols); k++) {

                        Leg leg = vars.makeYoYLeg(vars.evaluationDate,lengths[i]);

                        boost::shared_ptr<Instrument> cap
                        = vars.makeYoYCapFloor(YoYInflationCapFloor::Cap,
                                           leg, strikes[j], vols[k], whichPricer);

                        boost::shared_ptr<Instrument> floor
                        = vars.makeYoYCapFloor(YoYInflationCapFloor::Floor,
                                           leg, strikes[j], vols[k], whichPricer);

                        Date from = vars.nominalTS->referenceDate();
                        Date to = from+lengths[i]*Years;
                        Schedule yoySchedule = MakeSchedule().from(from).to(to)
                        .withTenor(1*Years)
                        .withCalendar(UnitedKingdom())
                        .withConvention(Unadjusted)
                        .backwards()
                        ;

                        YearOnYearInflationSwap swap(YearOnYearInflationSwap::Payer,
                                                    1000000.0,
                                                    yoySchedule,//fixed schedule, but same as yoy
                                                    strikes[j],
                                                    vars.dc,
                                                    yoySchedule,
                                                    vars.iir,
                                                    vars.observationLag,
                                                    0.0,        //spread on index
                                                    vars.dc,
                                                    UnitedKingdom());

                        Handle<YieldTermStructure> hTS(vars.nominalTS);
                        boost::shared_ptr<PricingEngine> sppe(new DiscountingSwapEngine(hTS));
                        swap.setPricingEngine(sppe);

                        // N.B. nominals are 10e6
                        if (std::fabs((cap->NPV()-floor->NPV()) - swap.NPV()) > 1.0e-6) {
                            BOOST_FAIL(
                                   "put/call parity violated:\n"
                                   << "    length:      " << lengths[i] << " years\n"
                                   << "    volatility:  " << io::volatility(vols[k]) << "\n"
                                   << "    strike:      " << io::rate(strikes[j]) << "\n"
                                   << "    cap value:   " << cap->NPV() << "\n"
                                   << "    floor value: " << floor->NPV() << "\n"
                                   << "    swap value:  " << swap.NPV());
                        }
                    }
                }
            }
        }
        // remove circular refernce
        vars.hy.linkTo(boost::shared_ptr<YoYInflationTermStructure>());
    }




    void InflationYoYCapFloorTest::testCachedValue() {

        BOOST_MESSAGE("Testing Black yoy inflation cap/floor price against cached values...");

        CommonVars vars;

        Size whichPricer = 0; // black

        Real K = 0.0295; // one centi-point is fair rate error i.e. < 1 cp
        Size j = 2;
        Leg leg = vars.makeYoYLeg(vars.evaluationDate,j);
        boost::shared_ptr<Instrument> cap
            = vars.makeYoYCapFloor(YoYInflationCapFloor::Cap,leg, K, 0.01, whichPricer);

        boost::shared_ptr<Instrument> floor
            = vars.makeYoYCapFloor(YoYInflationCapFloor::Floor,leg, K, 0.01, whichPricer);


        // close to atm prices
        Real cachedCapNPVblack   = 219.452;
        Real cachedFloorNPVblack =  314.641;
        // N.B. notionals are 10e6.
        BOOST_CHECK_MESSAGE(fabs(cap->NPV()-cachedCapNPVblack)<0.02,"yoy cap cached NPV wrong "
                            <<cap->NPV()<<" should be "<<cachedCapNPVblack<<" Black pricer"
                            <<" diff was "<<(fabs(cap->NPV()-cachedCapNPVblack)));
        BOOST_CHECK_MESSAGE(fabs(floor->NPV()-cachedFloorNPVblack)<0.02,"yoy floor cached NPV wrong "
                            <<floor->NPV()<<" should be "<<cachedFloorNPVblack<<" Black pricer"
                            <<" diff was "<<(fabs(floor->NPV()-cachedFloorNPVblack)));

        whichPricer = 1; // dd

        cap
        = vars.makeYoYCapFloor(YoYInflationCapFloor::Cap,leg, K, 0.01, whichPricer);

        floor
        = vars.makeYoYCapFloor(YoYInflationCapFloor::Floor,leg, K, 0.01, whichPricer);

        // close to atm prices
        Real cachedCapNPVdd   = 9114.61;
        Real cachedFloorNPVdd =  9209.8;
        // N.B. notionals are 10e6.
        BOOST_CHECK_MESSAGE(fabs(cap->NPV()-cachedCapNPVdd)<0.22,"yoy cap cached NPV wrong "
                            <<cap->NPV()<<" should be "<<cachedCapNPVdd<<" dd Black pricer"
                            <<" diff was "<<(fabs(cap->NPV()-cachedCapNPVdd)));
        BOOST_CHECK_MESSAGE(fabs(floor->NPV()-cachedFloorNPVdd)<0.22,"yoy floor cached NPV wrong "
                            <<floor->NPV()<<" should be "<<cachedFloorNPVdd<<" dd Black pricer"
                            <<" diff was "<<(fabs(floor->NPV()-cachedFloorNPVdd)));

        whichPricer = 2; // bachelier

        cap
        = vars.makeYoYCapFloor(YoYInflationCapFloor::Cap,leg, K, 0.01, whichPricer);

        floor
        = vars.makeYoYCapFloor(YoYInflationCapFloor::Floor,leg, K, 0.01, whichPricer);

        // close to atm prices
        Real cachedCapNPVbac   = 8852.4;
        Real cachedFloorNPVbac =  8947.59;
        // N.B. notionals are 10e6.
        BOOST_CHECK_MESSAGE(fabs(cap->NPV()-cachedCapNPVbac)<0.22,"yoy cap cached NPV wrong "
                            <<cap->NPV()<<" should be "<<cachedCapNPVbac<<" bac Black pricer"
                            <<" diff was "<<(fabs(cap->NPV()-cachedCapNPVbac)));
        BOOST_CHECK_MESSAGE(fabs(floor->NPV()-cachedFloorNPVbac)<0.22,"yoy floor cached NPV wrong "
                            <<floor->NPV()<<" should be "<<cachedFloorNPVbac<<" bac Black pricer"
                            <<" diff was "<<(fabs(floor->NPV()-cachedFloorNPVbac)));

        // remove circular refernce
        vars.hy.linkTo(boost::shared_ptr<YoYInflationTermStructure>());
    }


}


namespace NamespaceCPISwapTest {

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

        // setup
        CommonVars() {

            // option variables
            nominals = std::vector<Real>(1,1000000); // 1M
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

        struct InputParams {

            InputParams(){

            }
        };

    };

    bool checkAbsError(Real x1, Real x2, Real tolerance){
        return std::fabs(x1 - x2) < tolerance;
    }


    //! Classic inflation index ratio-from-base products.  Always have some "notional" flow.
    class CPISwapTest {
    public:
        //! consistency test w.r.t. manual build-up of calculatons
        void consistency();
        //! a CPI swap can look like a Zero Coupon Inflation Inedex Swap, check it gies same prices
        void zciisconsistency();
        //! one leg of a CPI swap looks like a CPI bond with growthOnly=true
        void cpibondconsistency();
        void myinflationcpiswap();
        void realInterestRateTS();
    };


    void CPISwapTest::consistency() {
        BOOST_MESSAGE("Check inflation leg vs calculation directly from inflation TS");

        CommonVars common;

        // ZeroInflationSwap aka CPISwap

        CPISwap::Type type = CPISwap::Payer;
        Real nominal = 1000000.0;
        bool subtractInflationNominal = true;
        // float+spread leg
        Spread spread = 0.0;
        DayCounter floatDayCount = Actual365Fixed();
        BusinessDayConvention floatPaymentConvention = ModifiedFollowing;
        Natural fixingDays = 0;
        boost::shared_ptr<IborIndex> floatIndex(new GBPLibor(Period(6,Months),
                                                             common.nominalTS));

        // fixed x inflation leg
        Rate fixedRate = 0.1;//1% would be 0.01
        Real baseCPI = 206.1; // would be 206.13871 if we were interpolating
        DayCounter fixedDayCount = Actual365Fixed();
        BusinessDayConvention fixedPaymentConvention = ModifiedFollowing;
        Calendar fixedPaymentCalendar = UnitedKingdom();
        boost::shared_ptr<ZeroInflationIndex> fixedIndex = common.ii;
        Period contractObservationLag = common.contractObservationLag;
        CPI::InterpolationType observationInterpolation = common.contractObservationInterpolation;

        // set the schedules
        Date startDate(2, October, 2007);
        Date endDate(2, October, 2052);
        Schedule floatSchedule = MakeSchedule().from(startDate).to(endDate)
        .withTenor(Period(6,Months))
        .withCalendar(UnitedKingdom())
        .withConvention(floatPaymentConvention)
        .backwards()
        ;
        Schedule fixedSchedule = MakeSchedule().from(startDate).to(endDate)
        .withTenor(Period(6,Months))
        .withCalendar(UnitedKingdom())
        .withConvention(Unadjusted)
        .backwards()
        ;


        CPISwap zisV(type, nominal, subtractInflationNominal,
                     spread, floatDayCount, floatSchedule,
                     floatPaymentConvention, fixingDays, floatIndex,
                     fixedRate, baseCPI, fixedDayCount, fixedSchedule,
                     fixedPaymentConvention, contractObservationLag,
                     fixedIndex, observationInterpolation); //

        Date asofDate = Settings::instance().evaluationDate();

        Real floatFix[] = {0.06255,0.05975,0.0637,0.018425,0.0073438,-1,-1};
        Real cpiFix[] = {211.4,217.2,211.4,213.4,-2,-2};

        for(Size i=0;i<floatSchedule.size(); i++){
            if (floatSchedule[i] < common.evaluationDate) {
                floatIndex->addFixing(floatSchedule[i], floatFix[i], true);//true=overwrite
            }

            boost::shared_ptr<CPICoupon>
            zic = boost::dynamic_pointer_cast<CPICoupon>(zisV.cpiLeg()[i]);
            if (zic) {
                if (zic->fixingDate() < (common.evaluationDate - Period(1,Months))) {
                    fixedIndex->addFixing(zic->fixingDate(), cpiFix[i], true);
                }
                //std::cout << "CPI swap fixing date " << i << ": " << zic->fixingDate() << std::endl;
            }
        }

        // simple structure so simple pricing engine - most work done by index
        boost::shared_ptr<DiscountingSwapEngine>
            dse(new DiscountingSwapEngine(common.nominalTS));

        zisV.setPricingEngine(dse);

        // get float+spread & fixed*inflation leg prices separately
        Real testInfLegNPV = 0.0;
        for(Size i=0;i<zisV.leg(0).size(); i++){

            Date zicPayDate = (zisV.leg(0))[i]->date();
            if(zicPayDate > asofDate) {
                testInfLegNPV += (zisV.leg(0))[i]->amount()*common.nominalTS->discount(zicPayDate);
            }

            boost::shared_ptr<CPICoupon>
                zicV = boost::dynamic_pointer_cast<CPICoupon>(zisV.cpiLeg()[i]);
            if (zicV) {
                Real diff = fabs( zicV->rate() - (fixedRate*(zicV->indexFixing()/baseCPI)) );
                QL_REQUIRE(diff<1e-8,"failed "<<i<<"th coupon reconstruction as "
                           << (fixedRate*(zicV->indexFixing()/baseCPI)) << " vs rate = "
                           <<zicV->rate() << ", with difference: " << diff);
            }
        }

        //std::cout << "ZCIIS float leg NPV: " << zisV.floatLegNPV() << std::endl;
        //std::cout << "ZCIIS fix   leg NPV: " << zisV.fixLegNPV() << std::endl;
        std::cout.imbue(std::locale("")); // uses thousands' separators
        std::cout << std::setw(25) << "ZCIIS paid     leg NPV(0): " << zisV.legNPV(0) << std::endl;
        std::cout << std::setw(25) << "ZCIIS received leg NPV(1): " << zisV.legNPV(1) << std::endl;
        std::cout << std::setw(25) << "ZCIIS NPV                : " << zisV.NPV() << std::endl;
        std::cout.imbue(std::locale()); // does not use thousands' separators

        Real error = fabs(testInfLegNPV - zisV.legNPV(0));
        QL_REQUIRE(error<1e-5,
                   "failed manual inf leg NPV calc vs pricing engine: " <<
                   testInfLegNPV << " vs " << zisV.legNPV(0));

        Real diff = fabs(1-zisV.NPV()/4191660.0);
        QL_REQUIRE(diff<1e-4, //QL_REQUIRE(diff<1e-5,
                   "failed stored consistency value test, ratio = " << diff);

        std::cout << "\nZCIIS fair rate: " << zisV.fairRate()   << std::endl;
        std::cout << "ZCIIS fair spread: " << zisV.fairSpread() << std::endl << std::endl;
    }


    void CPISwapTest::zciisconsistency() {
        BOOST_MESSAGE("One leg of a CPI swap looks like a CPI bond with growthOnly=true");

        CommonVars common;

        ZeroCouponInflationSwap::Type ztype = ZeroCouponInflationSwap::Payer;
        Real  nominal = 1000000.0;
        Date startDate(common.evaluationDate);
        Date endDate(25, November, 2059);
        Calendar cal = UnitedKingdom();
        BusinessDayConvention paymentConvention = ModifiedFollowing;
        DayCounter dummyDC, dc = ActualActual();
        Period observationLag(2,Months);

        Rate quote = 0.03714;
        ZeroCouponInflationSwap zciis(ztype, nominal, startDate, endDate, cal,
                                      paymentConvention, dc, quote, common.ii,
                                      observationLag);

        // simple structure so simple pricing engine - most work done by index
        boost::shared_ptr<DiscountingSwapEngine>
        dse(new DiscountingSwapEngine(common.nominalTS));

        zciis.setPricingEngine(dse);
        QL_REQUIRE(fabs(zciis.NPV())<1e-3,"zciis does not reprice to zero");

        std::cout.imbue(std::locale(""));
        std::cout << "ZCIIS NPV: " << zciis.NPV() << std::endl;



        std::vector<Date> oneDate;
        oneDate.push_back(endDate);
        Schedule schOneDate(oneDate, cal, paymentConvention);

        CPISwap::Type stype = CPISwap::Payer;
        Real inflationNominal = nominal;
        Real floatNominal = inflationNominal * std::pow(1.0+quote,50);
        bool subtractInflationNominal = true;
        Real dummySpread=0.0, dummyFixedRate=0.0;
        Natural fixingDays = 0;
        Date baseDate = startDate - observationLag;
        Real baseCPI = common.ii->fixing(baseDate);

        boost::shared_ptr<IborIndex> dummyFloatIndex;

        CPISwap cS(stype, floatNominal, subtractInflationNominal, dummySpread, dummyDC, schOneDate,
                   paymentConvention, fixingDays, dummyFloatIndex,
                   dummyFixedRate, baseCPI, dummyDC, schOneDate, paymentConvention, observationLag,
                   common.ii, CPI::AsIndex, inflationNominal);
        /*
        CPISwap(Type type,
                Real nominal,
                bool subtractInflationNominal,
                // float+spread leg
                Spread spread,
                const DayCounter& floatDayCount,
                const Schedule& floatSchedule,
                const BusinessDayConvention& floatRoll,
                Natural fixingDays,
                const boost::shared_ptr<IborIndex>& floatIndex,
                // fixed x inflation leg
                Rate fixedRate,
                Real baseCPI,
                const DayCounter& fixedDayCount,
                const Schedule& fixedSchedule,
                const BusinessDayConvention& fixedRoll,
                const Period& observationLag,
                const boost::shared_ptr<ZeroInflationIndex>& fixedIndex,
                CPI::InterpolationType observationInterpolation = CPI::AsIndex,
                Real inflationNominal = Null<Real>()
                );
          */

        cS.setPricingEngine(dse);
        QL_REQUIRE(fabs(cS.NPV())<1e-3,"CPISwap as ZCIIS does not reprice to zero");
        std::cout << "CPI Swap NPV: " << cS.NPV() << std::endl;

        for (Size i=0; i<2; i++) {
            QL_REQUIRE(fabs(cS.legNPV(i)-zciis.legNPV(i))<1e-3,"zciis leg does not equal CPISwap leg");
        }
        std::cout << "CPI swap NPV - ZCIIS NPV = difference" << std::endl;
        std::cout << "Receiver Leg: " << cS.legNPV(0) << " - " << zciis.legNPV(0) << " = " << (cS.legNPV(0)-zciis.legNPV(0)) << std::endl;
        std::cout << "Payer    Leg: " << cS.legNPV(1) << " - " << zciis.legNPV(1) << " = " << (cS.legNPV(1)-zciis.legNPV(1)) << std::endl;

        std::cout.imbue(std::locale());
    }


    void CPISwapTest::cpibondconsistency() {
        BOOST_MESSAGE("Inflation-indexed bond consistency check");

        CommonVars common;

        // ZeroInflationSwap aka CPISwap

        CPISwap::Type type = CPISwap::Payer;
        Real nominal = 1000000.0;
        bool subtractInflationNominal = true;
        // float+spread leg
        Spread spread = 0.0;
        DayCounter floatDayCount = Actual365Fixed();
        BusinessDayConvention floatPaymentConvention = ModifiedFollowing;
        Natural fixingDays = 0;
        boost::shared_ptr<IborIndex> floatIndex(new GBPLibor(Period(6,Months),
                                                             common.nominalTS));

        // fixed x inflation leg
        Rate fixedRate = 0.1;//1% would be 0.01
        Real baseCPI = 206.1; // would be 206.13871 if we were interpolating
        DayCounter fixedDayCount = Actual365Fixed();
        BusinessDayConvention fixedPaymentConvention = ModifiedFollowing;
        Calendar fixedPaymentCalendar = UnitedKingdom();
        boost::shared_ptr<ZeroInflationIndex> fixedIndex = common.ii;
        Period contractObservationLag = common.contractObservationLag;
        CPI::InterpolationType observationInterpolation = common.contractObservationInterpolation;

        // set the schedules
        Date startDate(2, October, 2007);
        Date endDate(2, October, 2052);
        Schedule floatSchedule = MakeSchedule().from(startDate).to(endDate)
        .withTenor(Period(6,Months))
        .withCalendar(UnitedKingdom())
        .withConvention(floatPaymentConvention)
        .backwards()
        ;
        Schedule fixedSchedule = MakeSchedule().from(startDate).to(endDate)
        .withTenor(Period(6,Months))
        .withCalendar(UnitedKingdom())
        .withConvention(Unadjusted)
        .backwards()
        ;


        CPISwap zisV(type, nominal, subtractInflationNominal,
                     spread, floatDayCount, floatSchedule,
                     floatPaymentConvention, fixingDays, floatIndex,
                     fixedRate, baseCPI, fixedDayCount, fixedSchedule,
                     fixedPaymentConvention, contractObservationLag,
                     fixedIndex, observationInterpolation);

        Real floatFix[] = {0.06255,0.05975,0.0637,0.018425,0.0073438,-1,-1};
        Real cpiFix[] = {211.4,217.2,211.4,213.4,-2,-2};
        for(Size i=0;i<floatSchedule.size(); i++){
            if (floatSchedule[i] < common.evaluationDate) {
                floatIndex->addFixing(floatSchedule[i], floatFix[i],true);//true=overwrite
            }

            boost::shared_ptr<CPICoupon>
            zic = boost::dynamic_pointer_cast<CPICoupon>(zisV.cpiLeg()[i]);
            if (zic) {
                if (zic->fixingDate() < (common.evaluationDate - Period(1,Months))) {
                    fixedIndex->addFixing(zic->fixingDate(), cpiFix[i],true);
                }
            }
        }


        // simple structure so simple pricing engine - most work done by index
        boost::shared_ptr<DiscountingSwapEngine>
        dse(new DiscountingSwapEngine(common.nominalTS));

        zisV.setPricingEngine(dse);

        // now do the bond equivalent
        std::vector<Rate> fixedRates(1,fixedRate);
        Natural settlementDays = 1;// cannot be zero!
        bool growthOnly = true;

        CPIBond cpiB(settlementDays, nominal, growthOnly,
                     baseCPI, contractObservationLag, fixedIndex,
                     observationInterpolation, fixedSchedule,
                     fixedRates, fixedDayCount, fixedPaymentConvention);

        boost::shared_ptr<DiscountingBondEngine>
        dbe(new DiscountingBondEngine(common.nominalTS));
        cpiB.setPricingEngine(dbe);

        QL_REQUIRE(fabs(cpiB.NPV() - zisV.legNPV(0))<1e-5,"cpi bond does not equal equivalent cpi swap leg");

        std::cout.imbue(std::locale(""));
        std::cout << "Inflation-indexed NPV =\t" << cpiB.NPV() << std::endl;
        std::cout << "CPI swap NPV =\t\t" << zisV.legNPV(0) << std::endl;
        std::cout << "Difference =\t\t" << (cpiB.NPV() - zisV.legNPV(0)) << std::endl;
        std::cout.imbue(std::locale());
    }

    class SimpleYield: public Observable{
    private:
        Rate yield_;
    public:
        SimpleYield(const Rate& yield):yield_(yield){
        }
        Rate getYield() const{return yield_;}

        void setYield(const Rate& yield){
            yield_=yield;
            // yield has changed, notify observers!
            notifyObservers();
        }
    };


    class SimpleDiscountFactor: public Observable, Observer{
    private:
        DiscountFactor df_;
        Time mat_;
        boost::shared_ptr<SimpleYield> y_;
    public:
        SimpleDiscountFactor(const boost::shared_ptr<SimpleYield>& y,
            Time& mat):y_(y),mat_(mat){
                // register yield as an observable!
                std::cout << "registerWith(y_)" << std::endl;
                registerWith(y_);
                df_ = exp(-y_->getYield()*mat_);
        }
        void update(){
            // something has changed, recalculate yield
            df_= exp(-y_->getYield()*mat_);
            std::cout << "notifyObservers()" << std::endl;
            notifyObservers();
        }
        Real getDiscount() const{
            return df_;
        }
    };

    class SimpleDiscountFactor1: public Observable, Observer{
    private:
        DiscountFactor df_;
        Date evalDate_,matDate_;
        boost::shared_ptr<SimpleYield> y_;
        DayCounter dc_;
    public:
        SimpleDiscountFactor1(const boost::shared_ptr<SimpleYield>& y,
            const Date& matDate, const DayCounter& dc)
            :y_(y),matDate_(matDate),dc_(dc){
                // register yield as an observable!
                evalDate_=Settings::instance().evaluationDate();
                registerWith(y_);
                registerWith(Settings::instance().evaluationDate());
                df_=exp(-y_->getYield()*dc_.yearFraction(evalDate_,matDate_));
        }
        void update(){
            // something has changed, recalculate discount factor
            evalDate_=Settings::instance().evaluationDate();
            df_=exp(-y_->getYield()*dc_.yearFraction(evalDate_,matDate_));
            notifyObservers();
        }
        Real getDiscount() const{
            return df_;
        }
    };

    void CPISwapTest::myinflationcpiswap() {


        /*
        LARGE_TITLE("Test observer-observable pattern");
        boost::shared_ptr<SimpleYield> myYield(new SimpleYield(0.03));
        Time mat = 1.0;
        SimpleDiscountFactor myDf(myYield, mat);
        std::cout << " Discount before update: " << myDf.getDiscount() << std::endl;
        myYield->setYield(0.01);
        std::cout << " Discount after update: " << myDf.getDiscount() << std::endl;


        LARGE_TITLE("Test observer-observable pattern 2 (yield change -> DF change -> discounted CF change)");
        boost::shared_ptr<SimpleYield> myYield(new SimpleYield(0.03));
        Date mat = Date::todaysDate()+12*Months;
        DayCounter dc=ActualActual();
        */



        BOOST_MESSAGE("ZCIIS test");

        CommonVars common;

        ZeroCouponInflationSwap::Type ztype = ZeroCouponInflationSwap::Payer;
        Real  nominal = 1000000.0;
        Date startDate(common.evaluationDate);
        Date endDate(25, November, 2059);
        Calendar cal = UnitedKingdom();
        BusinessDayConvention paymentConvention = ModifiedFollowing;
        DayCounter dummyDC, dc = ActualActual();
        Period observationLag(2,Months);
        Rate quote = 0.03714;

        ZeroCouponInflationSwap zciis(ztype, nominal, startDate, endDate, cal,
            paymentConvention, dc, quote, common.ii,
            observationLag);

        /*
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

        boost::shared_ptr<DiscountingSwapEngine>
            dse(new DiscountingSwapEngine(common.nominalTS));

        zciis.setPricingEngine(dse);
        QL_REQUIRE(fabs(zciis.NPV())<1e-3,"zciis does not reprice to zero");

        std::cout.imbue(std::locale(""));
        std::cout << "ZCIIS NPV: " << zciis.NPV() << std::endl;

        //std::cout << ": " <<  << std::endl;
        std::cout << "Nominal: " << zciis.nominal() << std::endl;
        std::cout << "Start Date: " << zciis.startDate() << std::endl;
        std::cout << "Maturity Date: " << zciis.maturityDate() << std::endl;
        std::cout << "Fixed Rate: " << zciis.fixedRate() << std::endl;
        std::cout << "Fixed Leg NPV: " << zciis.fixedLegNPV() << std::endl;
        std::cout << "Inflation Leg NPV: " << zciis.inflationLegNPV() << std::endl;
        std::cout << "ZCIIS Fair Rate: " << zciis.fairRate() << std::endl;
        int nYear = endDate.year() - startDate.year();

        std::cout << "\nZero Rates:" << std::endl;
        for(int i=0; i<nYear; ++i)
            std::cout << i << ": " << common.nominalTS->zeroRate(i, Compounded, Annual, false).rate() << std::endl;
        std::cout << "\nForward Rates:" << std::endl;
        for(int i=0; i<nYear-5; ++i)
            std::cout << i << ": " << common.nominalTS->forwardRate(startDate+(i+1)*Years, 1*Years, dc, Compounded, Annual, false) << std::endl;

        // dse->calculate();
        //boost::progress_display show_progress(10);
        //for (long i = 0; i < 10; ++i)
        //{
        //	dse->calculate(); // burn some time
        //	++show_progress;
        //}
        //std::cout << std::endl;



        /*
        SimpleDiscountFactor1 myDf(myYield, mat, dc);

        std::cout << "Discount before yield update:" << myDf.getDiscount() << std::endl;
        myYield->setYield(0.01);
        std::cout << "Discount after yield update:" << myDf.getDiscount() << std::endl;
        Settings::instance().evaluationDate() = mat - 1*Months;
        std::cout << "Discount after evaluation date update:" << myDf.getDiscount() << std::endl;

        std::cout << "Global evaluation date: " << Settings::instance().evaluationDate() << std::endl;
        */

    }

    void CPISwapTest::realInterestRateTS() {
        BOOST_MESSAGE("Implied Real Interest Rate Term Structure");

        CommonVars common;

        ZeroCouponInflationSwap::Type ztype = ZeroCouponInflationSwap::Payer;
        Real nominal = 1000000.0;
        Date startDate(common.evaluationDate);
        Date endDate(25, November, 2059);
        Calendar calendar = UnitedKingdom();
        BusinessDayConvention paymentConvention = ModifiedFollowing;
        DayCounter dc = ActualActual();
        Period observationLag(2,Months);

        Rate quote = 0.03714;
        ZeroCouponInflationSwap zciis(ztype, nominal, startDate, endDate, calendar,
                                      paymentConvention, dc, quote, common.ii,
                                      observationLag);

        // simple structure so simple pricing engine - most work done by index
        boost::shared_ptr<DiscountingSwapEngine>
        dse(new DiscountingSwapEngine(common.nominalTS));

        zciis.setPricingEngine(dse);

        QL_REQUIRE(fabs(zciis.NPV())<1e-3,"zciis does not reprice to zero");

        //std::cout.imbue(std::locale(""));
        std::cout << "ZCIIS NPV: " << zciis.NPV() << std::endl;
        std::cout.imbue(std::locale());


        std::cout << "\n(1) Nominal interest rates" << std::endl;
        std::cout << std::setw(24) << "Date"
                  << std::setw(10) << "DF"
                  << std::setw(10) << "spot"
                  << std::setw(10) << "fwd" << std::endl;
        Date d(1, December, 2009);
        for(Size i=0; i<24; ++i) {
            std::cout << std::setw(2) << i << ": "
                      << std::setw(10) << d
                      << std::setw(10) << common.nominalTS->discount(d, true)
                      << std::setw(10) << io::rate(common.nominalTS->zeroRate(d, Actual365Fixed(), Compounded, Monthly, true).rate())
                      << std::setw(10) << io::rate(common.nominalTS->forwardRate(d, d+1*Months, Actual365Fixed(), Compounded, Monthly, true).rate())
                      << std::endl;
            d += 1*Months;
        }

        std::cout << "\n(2) Inflation swap break-even fixed rates (CPI swap implied zero coupon inflation term structure)" << std::endl;
        d = Date(1, December, 2009);
        for(Size i=0; i<24; ++i) {
            std::cout << std::setw(2) << i << ": "
                      << std::setw(10) << d
                      << std::setw(10) << io::rate(common.cpiTS->zeroRate(d, common.observationLag, false, true))
                      << std::endl;
            d += 1*Months;
        }

        std::cout << "\n(3) Real interest rates term structure" << std::endl;
        std::cout << std::setw(24) << "Date"
                  << std::setw(10) << "nominal"
                  << std::setw(10) << "inflation"
                  << std::setw(10) << "real" << std::endl;
        d = Date(1, December, 2009);
        for(Size i=0; i<24; ++i) {
            Real zeronominalinterestrate = common.nominalTS->zeroRate(d, Actual365Fixed(), Compounded, Monthly, true).rate();
            Real zeroinflationrate = common.cpiTS->zeroRate(d, common.observationLag, false, true);
            Real zerorealinflationrate = zeronominalinterestrate - zeroinflationrate;
            std::cout << std::setw(2) << i << ": "
                      << std::setw(10) << d
                      << std::setw(10) << io::rate(zeronominalinterestrate)
                      << std::setw(10) << io::rate(zeroinflationrate)
                      << std::setw(10) << io::rate(zerorealinflationrate)
                      << std::endl;
            d += 1*Months;
        }

    }

}


namespace NamespaceYYIISTest {

    struct CommonVars {

        // local data globals
        Calendar calendar;
        Period observationLag;
        Date evaluationDate;

        Handle<YieldTermStructure> nominalEUR;
        RelinkableHandle<YoYInflationTermStructure> yoyEU;
        boost::shared_ptr<YoYInflationIndex> yoyIndexEU;

        SavedSettings backup;

        CommonVars() {

            // make sure of the evaluation date
            Date today(Day(23), Month(11), Year(2007));
            calendar = TARGET();
            evaluationDate = calendar.adjust(today);
            Settings::instance().evaluationDate() = evaluationDate;

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

            std::vector<Real> r;
            std::vector<Date> d;
            Size nTimesEUR = LENGTH(timesEUR);
            for (Size i = 0; i < nTimesEUR; i++) {
                r.push_back(ratesEUR[i]);
                Size ys = (Size)floor(timesEUR[i]);
                Size ds = (Size)((timesEUR[i]-(Real)ys)*365);
                Date dd = evaluationDate + Period(ys,Years) + Period(ds,Days);
                d.push_back( dd );
            }

            boost::shared_ptr<InterpolatedZeroCurve<Cubic> >
                euriborTS(new InterpolatedZeroCurve<Cubic>(d, r, Actual365Fixed()));
            Handle<YieldTermStructure> nominalHeur(euriborTS, false);
            nominalEUR = nominalHeur; // Handle<YieldTermStructure> nominalEUR


            // times = years - lag, where the lag is 2 months or 2/12
            // because this data is derived from cap/floor data that
            // is based on a 2 month lag.
            observationLag = Period(2, Months);

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
            Date baseDate = TARGET().advance(evaluationDate, -2, Months, ModifiedFollowing);
            for (Size i = 0; i < LENGTH(yoyEUrates); i++) {
                Date dd = TARGET().advance(baseDate, i, Years, ModifiedFollowing);
                d.push_back(dd);
                r.push_back(yoyEUrates[i]);
            }

            bool indexIsInterpolated = true;    // actually false for UKRPI but smooth surfaces are
                                                // better for finding intersections etc

            boost::shared_ptr<InterpolatedYoYInflationCurve<Linear> >
                pYTSEU( new InterpolatedYoYInflationCurve<Linear>(
                        evaluationDate, TARGET(), Actual365Fixed(), observationLag, Monthly,
                        indexIsInterpolated, nominalEUR, d, r));
            pYTSEU->update();
            yoyEU.linkTo(pYTSEU); // RelinkableHandle<T=YoYInflationTermStructure> yoyEU
        }

        void reset() {
            nominalEUR = Handle<YieldTermStructure>();
            yoyEU.linkTo(boost::shared_ptr<YoYInflationTermStructure>());
            yoyIndexEU.reset();
        }
    };


    class YYIISTest {
    public:
        void testYYIIS();
        void testYYIISFairRates();
    };

    void YYIISTest::testYYIIS() {

        BOOST_MESSAGE("YYIIS test");

        CommonVars common;

        std::cout.imbue(std::locale()); // unuse thousands' separators

        Date evaluationDate = Settings::instance().evaluationDate();
        std::cout << "Evaluation Date: " << evaluationDate << std::endl;

        YearOnYearInflationSwap::Type yyiistype = YearOnYearInflationSwap::Payer;
        Real nominal = 1000000.0;
        Rate fixedRate = 0.025737120876593; //0.023896588033158
        Rate spread = 0.0;
        DayCounter fixedDayCount = ActualActual();
        DayCounter yoyDayCount = ActualActual();
        Date from(20, July, 2007);
        Date to(20, November, 2036);
        Schedule fixedSchedule = MakeSchedule().from(from).to(to)
            .withTenor(1*Months)
            .withCalendar(common.calendar)
            .withConvention(ModifiedFollowing);
        Schedule yoySchedule = MakeSchedule().from(from).to(to)
            .withTenor(1*Months)
            .withCalendar(common.calendar)
            .withConvention(ModifiedFollowing);
        Calendar paymentCalendar = common.calendar;
        boost::shared_ptr<YoYInflationIndex> yoyIndex = common.yoyIndexEU;
        Period observationLag = common.observationLag;
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
            paymentConvention);

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
        */


        // Schedule yoySchedule
        std::cout << "\n1.YoY Schedule" << std::endl;
        for(Size i=0; i<yoySchedule.size(); i++){
            std::cout << "  " << yoySchedule[i] << std::endl;
        }

        // Schedule fixedSchedule
        std::cout << "\n2.Fixed Schedule" << std::endl;
        for(Size i=0; i<fixedSchedule.size(); i++){
            std::cout << "  " << fixedSchedule[i] << std::endl;
        }

        // boost::shared_ptr<YoYInflationIndex> yoyIndexUK
        // ->yoyInflationTermStructure();
        std::cout << "\n3.YoY Index" << std::endl;
        Date dd(20, November, 2007);
        for(Size i=0; i<20; ++i) {
            std::cout << std::setw(2) << i << ": " << "\t"
                      << std::setw(7) << dd << "\t"
                      << std::setw(7) << io::rate(common.yoyIndexEU->fixing(dd, true))
                      << std::endl;            
            dd += 1*Months;
        }

        // RelinkableHandle<YoYInflationTermStructure> yoyEU
        // yoyEU.linkTo(boost::shared_ptr<InterpolatedYoYInflationCurve<Linear> > pYTSEU)
        std::cout << "\n4.YoY Inflation Rate Term Structure (yoyEU)" << std::endl;
        Date ddd(20, July, 2007);
        for(Size i=0; i<26; ++i) {
            std::cout << std::setw(2) << i << ": " << "\t"
                      << std::setw(7) << ddd << "\t"
                      << std::setw(7) << std::setprecision(5) << io::rate(common.yoyEU->yoyRate(ddd, Period(-3, Months), false, true))
                      << std::endl;
            ddd += 1*Months;
        }

        // Handle<YieldTermStructure> nominalEUR
        std::cout << "\n5.European Nominal Interest Rate Term Structure (nominalEUR)" << std::endl;
        std::cout << std::setw(25) << "Date"
                  << std::setw(7) << "DF"
                  << std::setw(7) << "spot"
                  << std::setw(7) << "fwd" << std::endl;
        Date d(20, December, 2007);
        for(Size i=0; i<30; ++i) {
            std::cout << std::setw(2) << i << ": "
                      << std::setw(10) << d
                      << std::setw(7) << std::setprecision(5) << common.nominalEUR->discount(d, true)
                      << std::setw(7) << std::setprecision(5) << io::rate(common.nominalEUR->zeroRate(d, Actual365Fixed(), Compounded, Monthly, true).rate())
                      << std::setw(7) << std::setprecision(5) << io::rate(common.nominalEUR->forwardRate(d, d+1*Months, Actual365Fixed(), Compounded, Monthly, true).rate())
                      << std::endl;
            d += 1*Months;
        }

        boost::shared_ptr<DiscountingSwapEngine>
            yyiisdse(new DiscountingSwapEngine(common.nominalEUR));

        yyiis.setPricingEngine(yyiisdse);
        //yyiis.recalculate();

        //QL_REQUIRE(fabs(yyiis.NPV())<1e-3,"yyiis does not reprice to zero");

        std::cout << "\nEvaluation Date: "    << evaluationDate      << std::endl;
        std::cout << "YYIIS yoy leg NPV  : "  << yyiis.yoyLegNPV()   << std::endl;
        std::cout << "YYIIS fixed leg NPV: "  << yyiis.fixedLegNPV() << std::endl;
        std::cout << "YYIIS NPV: "            << yyiis.NPV()         << std::endl;
        std::cout << "YYIIS fair swap rate: " << std::setprecision(15)
                  << yyiis.fairRate()    << std::endl;

        common.reset();

    }

    void YYIISTest::testYYIISFairRates() {

        BOOST_MESSAGE("YYIIS fair fixed rates test");

        CommonVars common;

        std::cout.imbue(std::locale()); // unuse thousands' separators

        Date evaluationDate = Settings::instance().evaluationDate();
        std::cout << "Evaluation Date: " << evaluationDate << std::endl;

        YearOnYearInflationSwap::Type yyiistype = YearOnYearInflationSwap::Payer;
        Real nominal = 1000000.0;
        Rate fixedRate = 0.023896588; //0.023896588033158
        Rate spread = 0.0;
        DayCounter fixedDayCount = ActualActual();
        DayCounter yoyDayCount = ActualActual();
        Date from(20, July, 2007);
        Date to(20, November, 2036);
        Schedule fixedSchedule = MakeSchedule().from(from).to(to)
            .withTenor(1*Months)
            .withCalendar(common.calendar)
            .withConvention(ModifiedFollowing);
        Schedule yoySchedule = MakeSchedule().from(from).to(to)
            .withTenor(1*Months)
            .withCalendar(common.calendar)
            .withConvention(ModifiedFollowing);
        Calendar paymentCalendar = common.calendar;
        boost::shared_ptr<YoYInflationIndex> yoyIndex = common.yoyIndexEU;
        Period observationLag = common.observationLag;
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
            paymentConvention);

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
        */




        // Schedule yoySchedule
        std::cout << "\n1.YoY Schedule" << std::endl;
        for(Size i=0; i<yoySchedule.size(); i++){
            std::cout << "  " << yoySchedule[i] << std::endl;
        }

        // Schedule fixedSchedule
        std::cout << "\n2.Fixed Schedule" << std::endl;
        for(Size i=0; i<fixedSchedule.size(); i++){
            std::cout << "  " << fixedSchedule[i] << std::endl;
        }

        // boost::shared_ptr<YoYInflationIndex> yoyIndexUK
        // ->yoyInflationTermStructure();
        std::cout << "\n3.YoY Index" << std::endl;
        Date dd(20, November, 2007);
        for(Size i=0; i<20; ++i) {
            std::cout << std::setw(2) << i << ": " << "\t"
                      << std::setw(10) << dd << "\t"
                      << std::setw(7) << io::rate(common.yoyIndexEU->fixing(dd, true))
                      << std::endl;
            dd += 1*Months;
        }

        // RelinkableHandle<YoYInflationTermStructure> yoyEU
        // yoyEU.linkTo(boost::shared_ptr<InterpolatedYoYInflationCurve<Linear> > pYTSEU)
        std::cout << "\n4.YoY Inflation Rate Term Structure (yoyEU)" << std::endl;
        Date ddd(20, July, 2007);
        for(Size i=0; i<26; ++i) {
            std::cout << std::setw(2) << i << ": " << "\t"
                      << std::setw(10) << ddd << "\t"
                      << std::setw(7) << io::rate(common.yoyEU->yoyRate(ddd, Period(-3, Months), false, true))
                      << std::endl;
            ddd += 1*Months;
        }

        // Handle<YieldTermStructure> nominalEUR
        std::cout << "\n5.European Nominal Interest Rate Term Structure (nominalEUR)" << std::endl;
        std::cout << std::setw(25) << "Date"
                  << std::setw(7) << "DF"
                  << std::setw(7) << "spot"
                  << std::setw(7) << "fwd" << std::endl;
        Date d(20, December, 2007);
        for(Size i=0; i<30; ++i) {
            std::cout << std::setw(2) << i << ": "
                      << std::setw(10) << d
                      << std::setw(7) << common.nominalEUR->discount(d, true)
                      << std::setw(7) << io::rate(common.nominalEUR->zeroRate(d, Actual365Fixed(), Compounded, Monthly, true).rate())
                      << std::setw(7) << io::rate(common.nominalEUR->forwardRate(d, d+1*Months, Actual365Fixed(), Compounded, Monthly, true).rate())
                      << std::endl;
            d += 1*Months;
        }

        boost::shared_ptr<DiscountingSwapEngine>
            yyiisdse(new DiscountingSwapEngine(common.nominalEUR));

        yyiis.setPricingEngine(yyiisdse);
        //yyiis.recalculate();

        //QL_REQUIRE(fabs(yyiis.NPV())<1e-3,"yyiis does not reprice to zero");

        std::cout << "\nEvaluation Date: "    << evaluationDate      << std::endl;
        std::cout << "YYIIS yoy leg NPV  : "  << yyiis.yoyLegNPV()   << std::endl;
        std::cout << "YYIIS fixed leg NPV: "  << yyiis.fixedLegNPV() << std::endl;
        std::cout << "YYIIS NPV: "            << yyiis.NPV()         << std::endl;
        std::cout << "YYIIS fair swap rate: " << std::setprecision(15)
                  << io::rate(yyiis.fairRate()) << std::endl;


        //reprice yyiis using the new fixed fair rate
        Real newFixedRate = yyiis.fairRate();
        yyiis = YearOnYearInflationSwap(
                    yyiistype,
                    nominal,
                    fixedSchedule,
                    newFixedRate, //fixedRate,
                    fixedDayCount,
                    yoySchedule,
                    yoyIndex,
                    observationLag,
                    spread,
                    yoyDayCount,
                    paymentCalendar,
                    paymentConvention);
        yyiis.setPricingEngine(yyiisdse);
        std::cout << "\nRepriced YYIIS" << std::endl;
        std::cout << "Evaluation Date: "    << evaluationDate      << std::endl;
        std::cout << "YYIIS yoy leg NPV  : "  << yyiis.yoyLegNPV()   << std::endl;
        std::cout << "YYIIS fixed leg NPV: "  << yyiis.fixedLegNPV() << std::endl;
        std::cout << "YYIIS NPV: "            << yyiis.NPV()         << std::endl;
        std::cout << "YYIIS fair swap rate: " << std::setprecision(15)
                  << io::rate(yyiis.fairRate()) << std::endl;

        common.reset();

    }

}



///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

int main(int, char* []) {

    try {

        boost::timer timer;

        if(true)
        {
            LARGE_TITLE("Testing Inflation Volatility...");
            using namespace NamespaceInflationVolTest;
            InflationVolTest test;
            test.testYoYPriceSurfaceToATM();
            test.testYoYPriceSurfaceToVol();

            setup();
            boost::shared_ptr<YoYCapFloorTermPriceSurface> capFloorPrices = priceSurfEU;
            boost::shared_ptr<Matrix> cPriceEU_ = cPriceEU;
            boost::shared_ptr<Matrix> fPriceEU_ = fPriceEU;

            std::cout << "\n\ncapPricesEU[ncStrikesEU][ncfMaturitiesEU]" << std::endl;
            for(Size i = 0; i < cPriceEU_->rows(); i++) {
                if(i==0) {
                    std::cout << std::setw(8) << "K\\tau";
                    for(Size j = 0; j < cPriceEU_->columns(); j++) {
                        std::cout << std::setw(9) << cfMaturitiesEU[j];
                    }
                    std::cout << std::endl;
                }
                for(Size j = 0; j < cPriceEU_->columns(); j++) {
                    if(j==0) {
                        std::cout << std::setw(8) << std::setprecision(3) << cStrikesEU[i];
                    }
                    std::cout << std::setprecision(3)
                              << setiosflags(ios::fixed)
                              << std::setw(10)
                              << (*cPriceEU_)[i][j];
                }
                std::cout << std::endl;
            }

            std::cout << "\n\nfloorPricesEU[nfStrikesEU][ncfMaturitiesEU]" << std::endl;
            for(Size i = 0; i < fPriceEU_->rows(); i++) {
                if(i==0) {
                    std::cout << std::setw(8) << "K\\tau";
                    for(Size j = 0; j < fPriceEU_->columns(); j++) {
                        std::cout << std::setw(9) << cfMaturitiesEU[j];
                    }
                    std::cout << std::endl;
                }
                for(Size j = 0; j < fPriceEU_->columns(); j++) {
                    if(j==0) {
                        std::cout << std::setw(8) << std::setprecision(3) << fStrikesEU[i];
                    }
                    std::cout << std::setprecision(3)
                              << setiosflags(ios::fixed)
                              << std::setw(10)
                              << (*fPriceEU_)[i][j];
                }
                std::cout << std::endl;
            }

            RelinkableHandle<YoYInflationTermStructure> yoyEU_ = yoyEU;
            Handle<YieldTermStructure> nominalGBP_ = nominalGBP;


            setupPriceSurface();
            boost::shared_ptr<InterpolatedYoYCapFloorTermPriceSurface<Bicubic,Cubic> > priceSurfEU_ = priceSurfEU;

        }


        if(true)
        {
            LARGE_TITLE("My inflation volatility test");
            using namespace NamespaceInflationVolTest;
            setup();
            setupPriceSurface();

            std::pair<std::vector<Time>, std::vector<Rate> >
                    timerates = priceSurfEU->atmYoYSwapTimeRates();
            std::pair<std::vector<Date>, std::vector<Rate> >
                    daterates = priceSurfEU->atmYoYSwapDateRates();
            std::cout << "YoY inflation atm swap rates" << std::endl;
            std::cout << "Time\tDate\t\tRate" << std::endl;
            for (Size i=0; i<timerates.first.size(); ++i) {
                std::cout << timerates.first[i] << "\t"
                          << daterates.first[i] << "\t"
                          << timerates.second[i]
                          << std::endl;
            }

            pair<vector<Time>, vector<Rate> > yyATMt = priceSurfEU->atmYoYSwapTimeRates();
            pair<vector<Date>, vector<Rate> > yyATMd = priceSurfEU->atmYoYSwapDateRates();
            const Real crv[] = {0.024586, 0.0247575, 0.0249396, 0.0252596,
                                  0.0258498, 0.0262883, 0.0267915};
            const Real swaps[] = {0.024586, 0.0247575, 0.0249396, 0.0252596,
                                  0.0258498, 0.0262883, 0.0267915};
            const Real ayoy[] = {0.0247659, 0.0251437, 0.0255945, 0.0265234,
                                   0.0280457, 0.0285534, 0.0295884};
            std::cout << "\nATM yoy inflation swap rates (time)" << std::endl;
            for(Size i = 0; i < yyATMt.first.size(); i++) {
                std::cout << timerates.first[i] << "\t"
                          << daterates.first[i] << "\t"
                          << io::rate(yyATMt.second[i])
                          << std::endl;

            }
            std::cout << "\nATM yoy inflation swap rates (date)" << std::endl;
            for(Size i = 0; i < yyATMd.first.size(); i++) {
                std::cout << timerates.first[i] << "\t"
                          << daterates.first[i] << "\t"
                          << io::rate(priceSurfEU->atmYoYSwapRate(yyATMd.first[i]))
                          << std::endl;

            }
            std::cout << "\nATM yoy inflation rates" << std::endl;
            for(Size i = 0; i < yyATMd.first.size(); i++) {
                std::cout << timerates.first[i] << "\t"
                          << daterates.first[i] << "\t"
                          << io::rate(priceSurfEU->atmYoYRate(yyATMd.first[i]))
                          << std::endl;
            }

            boost::shared_ptr<YoYInflationTermStructure>
                    yoyts = priceSurfEU->YoYTS();


        }


        if(true)
        {
            LARGE_TITLE("Testing Inflation CPI Cap/Floor...");
            using namespace NamespaceInflationCPICapFloorTest;
            InflationCPICapFloorTest test2;
            test2.cpicapfloorpricesurface();
            test2.cpicapfloorpricer();
        }


        if(true)
        {
            LARGE_TITLE("CPI Swap (ZCIIS)");
            using namespace NamespaceCPISwapTest;
            CPISwapTest cpiswaptest;
            cpiswaptest.consistency();
            cpiswaptest.zciisconsistency();
            cpiswaptest.cpibondconsistency();
            cpiswaptest.myinflationcpiswap();
            cpiswaptest.realInterestRateTS();
        }


        if(true)
        {
            LARGE_TITLE("Inflation YoY Cap/Floor");
            using namespace NamespaceInflationYoYCapFloorTest;
            //InflationYoYCapFloorTest inflyoycapfloortest;
            InflationYoYCapFloorTest::testConsistency();
            InflationYoYCapFloorTest::testParity();
            InflationYoYCapFloorTest::testCachedValue();
        }




//        {
//            LARGE_TITLE("Inflation Test");
//            InflationTest::testPeriod();
//            InflationTest::testZeroIndex();
//            InflationTest::testZeroTermStructure();
//            InflationTest::testYYIndex();
//            InflationTest::testYYTermStructure();
//            InflationTest::myInflationTest();
//        }



        if(true)
        {
            LARGE_TITLE("Year-on-Year Inflation Indexed Swap (YYIIS)");
            using namespace NamespaceYYIISTest;
            YYIISTest yyiistest;
            yyiistest.testYYIIS();
            yyiistest.testYYIISFairRates();
        }


        if(true)
        {
            LARGE_TITLE("Custom Year-on-Year Inflation Indexed Swap (YYIIS)");
            using namespace NamespaceYYIISTest;

            CommonVars common;

            std::cout.imbue(std::locale()); // unuse thousands' separators

            Date evaluationDate = Settings::instance().evaluationDate();

            YearOnYearInflationSwap::Type yyiistype = YearOnYearInflationSwap::Payer;
            Real nominal = 1000000.0;
            Rate fixedRate = 0.0257371208765921; //0.023896588033158
            Rate spread = 0.0;
            DayCounter fixedDayCount = ActualActual();
            DayCounter yoyDayCount = ActualActual();
            Date from(20, July, 2007);
            Date to(20, November, 2036);
            Schedule fixedSchedule = MakeSchedule().from(from).to(to)
                .withTenor(1*Months)
                .withCalendar(common.calendar)
                .withConvention(ModifiedFollowing);
            Schedule yoySchedule = MakeSchedule().from(from).to(to)
                .withTenor(1*Months)
                .withCalendar(common.calendar)
                .withConvention(ModifiedFollowing);
            Calendar paymentCalendar = common.calendar;
            boost::shared_ptr<YoYInflationIndex> yoyIndex = common.yoyIndexEU;
            Period observationLag = common.observationLag;
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
                paymentConvention);

            boost::shared_ptr<DiscountingSwapEngine>
                yyiisdse(new DiscountingSwapEngine(common.nominalEUR));

            yyiis.setPricingEngine(yyiisdse);
            //yyiis.recalculate();

            std::cout << "\nEvaluation Date: "    << evaluationDate      << std::endl;
            std::cout << "YYIIS yoy leg NPV  : "  << yyiis.yoyLegNPV()   << std::endl;
            std::cout << "YYIIS fixed leg NPV: "  << yyiis.fixedLegNPV() << std::endl;
            std::cout << "YYIIS NPV: "            << yyiis.NPV()         << std::endl;
            std::cout << "YYIIS fair swap rate: " << std::setprecision(15)
                      << yyiis.fairRate()    << std::endl;

            QL_REQUIRE(fabs(yyiis.NPV())<1e-3,"yyiis does not reprice to zero");

            common.reset();

        }


        if(true)
        {
            LARGE_TITLE("Minimization");

            class RosenBrockFunction : public CostFunction {
            public :
                Real value(const Array& x) const {
                    QL_REQUIRE (x.size ()==2 , " Rosenbrock function is 2- dim .");
                    Real res = (1 - x[0])*(1 - x[0]);
                    res += 100.0 * (x[1] - x[0] * x[0])*(x[1] - x[0] * x[0]);
                    return res;
                }
                Disposable <Array> values(const Array& x) const {
                    QL_REQUIRE(x.size ()==2, "Rosenbrock function is 2- dim.");
                    // irrelevant what you write in res for most of the optimizers
                    // most of them are using value anyways . try with res [0]=100.0
                    Array res(1); res[0]= value(x);
                    return res;
                }
            };

            Size maxIterations = 1000;
            Size minStatIterations = 100;
            Real rootEpsilon = 1e-8;
            Real functionEpsilon = 1e-9;
            Real gradientNormEpsilon = 1e-5;
            EndCriteria myEndCrit(maxIterations ,
                                minStatIterations,
                                rootEpsilon,
                                functionEpsilon,
                                gradientNormEpsilon);

            RosenBrockFunction myFunc;

            NoConstraint constraint;

            Problem myProb1(myFunc, constraint, Array(2 ,0.1));
            Problem myProb2(myFunc, constraint, Array(2 ,0.1));

            Simplex solver1(0.1);

            ConjugateGradient solver2 ;

            EndCriteria::Type solvedCrit1 = solver1.minimize(myProb1, myEndCrit);
            EndCriteria::Type solvedCrit2 = solver2.minimize(myProb2, myEndCrit);

            std::cout << " Criteria Simplex :" << solvedCrit1 << std::endl;
            std::cout << " Root Simplex :" << myProb1.currentValue() << std::endl;
            std::cout << " Min F Value Simplex :" << myProb1.functionValue() << std::endl;
            std::cout << " Criteria CG:" << solvedCrit2 << std::endl;
            std::cout << " Root CG:" << myProb2.currentValue() << std::endl;
            std::cout << " Min F Value CG :" << myProb2.functionValue() << std::endl;

        }




        {


        }



        Real seconds = timer.elapsed();
        Integer hours = int(seconds/3600);
        seconds -= hours * 3600;
        Integer minutes = int(seconds/60);
        seconds -= minutes * 60;
        std::cout << " \nRun completed in ";
        if (hours > 0)
            std::cout << hours << " h ";
        if (hours > 0 || minutes > 0)
            std::cout << minutes << " m ";
        std::cout << std::fixed << std::setprecision(0)
                  << seconds << " s\n" << std::endl;
        return 0;

    } catch (std::exception& e) {
        std::cout << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cout << "unknown error" << std::endl;
        return 1;
    }

}


