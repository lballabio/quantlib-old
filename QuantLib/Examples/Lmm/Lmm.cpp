#include <ql/quantlib.hpp>
#include <boost/lambda/lambda.hpp>
#include <iostream>

using namespace QuantLib;

const double LTCORR = 0.20;    // long term correlations
const double CORRDECAY = 0.10; // correlation decay

const Size FACTORS = 10; // number of factors
const long SEED = 42;    // seed for rng

const Size CALIBRATIONPATHS = 2500; // simulation paths in calibration
const Size PRICINGPATHS = 15000;      // simulation paths in pricing
const Size TRAININGPATHS = 15000;     // simulation paths for LS training

const bool stochvol = false; // use the stochvol model ?
const bool sobol = true; // use sobol rng?


// compute correlation between basis functions fwd and cms (forward, cms rate)
Real correlation(std::vector<std::vector<NodeData> >& simulationData) {
    SequenceStatistics stats(2);
    std::vector<Real> temp(2);
    for(Size j=0;j<simulationData[1].size();j++) {
        temp[0] = simulationData[1][j].values[1];
        temp[1] = simulationData[1][j].values[3];
        stats.add(temp);
    }
    return stats.correlation()[0][1];
}


int main(int, char **) {

    bool calibrate = false;   // calibrate the model ?
    bool price = !calibrate;      // price OTC swap ?

    std::cout << "Libor Market Model Demo --- CMS Swaption Pricing"
              << std::endl;

    // set evalulation date

    Date evalDate = Date(4, February, 2014);
    Date settlDate = TARGET().advance(evalDate, 2 * Days);
    Settings::instance().evaluationDate() = evalDate;

    // set up the interest rate curve (6m curve with deposit on short end to fix
    // a unique curve for everything)

    Real irquotes[] = {
        0.0013,  0.0013,  0.0013,   0.0016,                   // ON, TN, SN, SW
        0.0019,  0.0022,  0.0025,   0.0028,  0.0031,  0.0035, // 1M ... 6M Depo
        0.00363, 0.00357, 0.000356, 0.00355, 0.00359, // 1-7 ... 5-11 FRA
        0.00381,                                      // 1y Swap
        0.0037,  0.00381, 0.00394,  0.00407, 0.00421, // 7-13 ... 11-17 FRA
        0.00401,                                      // 18m Swap
        0.00456, 0.00474, 0.00494,  0.00515, 0.0054,  // 13-19 ... 17-23 FRA
        0.00445, 0.00598, 0.0081,   0.01028,          // 2y-5y Swap
        0.01242, 0.01437, 0.01613,  0.0177,  0.01908, // 6y- 10y Swap
        0.02028, 0.02131, 0.02219,  0.02292, 0.02351, 0.02398,
        0.02434, // 11y-17y Swap
        0.02462, 0.02483, 0.02499,  0.02512, 0.02522, 0.02529,
        0.02535,                                              // 18y-24y Swap
        0.02538, 0.0254,  0.02541,  0.02541, 0.0254,  0.0254, // 25y-30y Swap
        0.02545, 0.02558, 0.02569,  0.02582 // 35, 40, 50, 60y Swap
    };

    boost::shared_ptr<IborIndex> euribor6m(new Euribor(6 * Months));

    boost::shared_ptr<RateHelper> rhon = boost::make_shared<DepositRateHelper>(
        irquotes[0], 1 * Days, 0, TARGET(), Following, false, Actual360());
    boost::shared_ptr<RateHelper> rhtn = boost::make_shared<DepositRateHelper>(
        irquotes[1], 1 * Days, 1, TARGET(), Following, false, Actual360());
    boost::shared_ptr<RateHelper> rhsn = boost::make_shared<DepositRateHelper>(
        irquotes[2], 1 * Days, 2, TARGET(), Following, false, Actual360());
    boost::shared_ptr<RateHelper> rhsw = boost::make_shared<DepositRateHelper>(
        irquotes[3], 1 * Weeks, 2, TARGET(), Following, false, Actual360());
    boost::shared_ptr<RateHelper> rh1m = boost::make_shared<DepositRateHelper>(
        irquotes[4], 1 * Months, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<RateHelper> rh2m = boost::make_shared<DepositRateHelper>(
        irquotes[5], 2 * Months, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<RateHelper> rh3m = boost::make_shared<DepositRateHelper>(
        irquotes[6], 3 * Months, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<RateHelper> rh4m = boost::make_shared<DepositRateHelper>(
        irquotes[7], 4 * Months, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<RateHelper> rh5m = boost::make_shared<DepositRateHelper>(
        irquotes[8], 5 * Months, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<RateHelper> rh6m = boost::make_shared<DepositRateHelper>(
        irquotes[9], 6 * Months, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<FraRateHelper> rh1 = boost::make_shared<FraRateHelper>(
        irquotes[10], 1, 7, 2, TARGET(), ModifiedFollowing, false, Actual360());
    boost::shared_ptr<FraRateHelper> rh2 = boost::make_shared<FraRateHelper>(
        irquotes[11], 2, 8, 2, TARGET(), ModifiedFollowing, false, Actual360());
    boost::shared_ptr<FraRateHelper> rh3 = boost::make_shared<FraRateHelper>(
        irquotes[12], 3, 9, 2, TARGET(), ModifiedFollowing, false, Actual360());
    boost::shared_ptr<FraRateHelper> rh4 = boost::make_shared<FraRateHelper>(
        irquotes[13], 4, 10, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<FraRateHelper> rh5 = boost::make_shared<FraRateHelper>(
        irquotes[14], 5, 11, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<SwapRateHelper> rh1y = boost::make_shared<SwapRateHelper>(
        irquotes[15], 1 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    boost::shared_ptr<FraRateHelper> rh7 = boost::make_shared<FraRateHelper>(
        irquotes[16], 7, 13, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<FraRateHelper> rh8 = boost::make_shared<FraRateHelper>(
        irquotes[17], 8, 14, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<FraRateHelper> rh9 = boost::make_shared<FraRateHelper>(
        irquotes[18], 9, 15, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<FraRateHelper> rh10 = boost::make_shared<FraRateHelper>(
        irquotes[19], 10, 16, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<FraRateHelper> rh11 = boost::make_shared<FraRateHelper>(
        irquotes[20], 11, 17, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<SwapRateHelper> rh18m =
        boost::make_shared<SwapRateHelper>(irquotes[21], 18 * Months, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<FraRateHelper> rh13 = boost::make_shared<FraRateHelper>(
        irquotes[22], 13, 19, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<FraRateHelper> rh14 = boost::make_shared<FraRateHelper>(
        irquotes[23], 14, 20, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<FraRateHelper> rh15 = boost::make_shared<FraRateHelper>(
        irquotes[24], 15, 21, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<FraRateHelper> rh16 = boost::make_shared<FraRateHelper>(
        irquotes[25], 16, 22, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<FraRateHelper> rh17 = boost::make_shared<FraRateHelper>(
        irquotes[26], 17, 23, 2, TARGET(), ModifiedFollowing, false,
        Actual360());
    boost::shared_ptr<SwapRateHelper> rh2y = boost::make_shared<SwapRateHelper>(
        irquotes[27], 2 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh3y = boost::make_shared<SwapRateHelper>(
        irquotes[28], 3 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh4y = boost::make_shared<SwapRateHelper>(
        irquotes[29], 4 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh5y = boost::make_shared<SwapRateHelper>(
        irquotes[30], 5 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh6y = boost::make_shared<SwapRateHelper>(
        irquotes[31], 6 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh7y = boost::make_shared<SwapRateHelper>(
        irquotes[32], 7 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh8y = boost::make_shared<SwapRateHelper>(
        irquotes[33], 8 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh9y = boost::make_shared<SwapRateHelper>(
        irquotes[34], 9 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh10y =
        boost::make_shared<SwapRateHelper>(irquotes[35], 10 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh11y =
        boost::make_shared<SwapRateHelper>(irquotes[36], 11 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh12y =
        boost::make_shared<SwapRateHelper>(irquotes[37], 12 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh13y =
        boost::make_shared<SwapRateHelper>(irquotes[38], 13 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh14y =
        boost::make_shared<SwapRateHelper>(irquotes[39], 14 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh15y =
        boost::make_shared<SwapRateHelper>(irquotes[40], 15 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh16y =
        boost::make_shared<SwapRateHelper>(irquotes[41], 16 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh17y =
        boost::make_shared<SwapRateHelper>(irquotes[42], 17 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh18y =
        boost::make_shared<SwapRateHelper>(irquotes[43], 18 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh19y =
        boost::make_shared<SwapRateHelper>(irquotes[44], 19 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh20y =
        boost::make_shared<SwapRateHelper>(irquotes[45], 20 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh21y =
        boost::make_shared<SwapRateHelper>(irquotes[46], 21 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh22y =
        boost::make_shared<SwapRateHelper>(irquotes[47], 22 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh23y =
        boost::make_shared<SwapRateHelper>(irquotes[48], 23 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh24y =
        boost::make_shared<SwapRateHelper>(irquotes[49], 24 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh25y =
        boost::make_shared<SwapRateHelper>(irquotes[50], 25 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh26y =
        boost::make_shared<SwapRateHelper>(irquotes[51], 26 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh27y =
        boost::make_shared<SwapRateHelper>(irquotes[52], 27 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh28y =
        boost::make_shared<SwapRateHelper>(irquotes[53], 28 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh29y =
        boost::make_shared<SwapRateHelper>(irquotes[54], 29 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh30y =
        boost::make_shared<SwapRateHelper>(irquotes[55], 30 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh35y =
        boost::make_shared<SwapRateHelper>(irquotes[56], 35 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh40y =
        boost::make_shared<SwapRateHelper>(irquotes[57], 40 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh50y =
        boost::make_shared<SwapRateHelper>(irquotes[58], 50 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);
    boost::shared_ptr<SwapRateHelper> rh60y =
        boost::make_shared<SwapRateHelper>(irquotes[59], 60 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6m);

    std::vector<boost::shared_ptr<RateHelper> > irratehelpers;

    irratehelpers.push_back(rhon);
    irratehelpers.push_back(rhtn);
    irratehelpers.push_back(rhsn);
    irratehelpers.push_back(rhsw);
    irratehelpers.push_back(rh1m);
    irratehelpers.push_back(rh2m);
    irratehelpers.push_back(rh3m);
    irratehelpers.push_back(rh4m);
    irratehelpers.push_back(rh5m);
    irratehelpers.push_back(rh6m);
    irratehelpers.push_back(rh1);
    irratehelpers.push_back(rh2);
    irratehelpers.push_back(rh3);
    irratehelpers.push_back(rh4);
    irratehelpers.push_back(rh5);
    irratehelpers.push_back(rh1y);
    irratehelpers.push_back(rh7);
    irratehelpers.push_back(rh8);
    irratehelpers.push_back(rh9);
    irratehelpers.push_back(rh10);
    irratehelpers.push_back(rh11);
    irratehelpers.push_back(rh18m);
    irratehelpers.push_back(rh13);
    irratehelpers.push_back(rh14);
    irratehelpers.push_back(rh15);
    irratehelpers.push_back(rh16);
    irratehelpers.push_back(rh17);
    irratehelpers.push_back(rh2y);
    irratehelpers.push_back(rh3y);
    irratehelpers.push_back(rh4y);
    irratehelpers.push_back(rh5y);
    irratehelpers.push_back(rh6y);
    irratehelpers.push_back(rh7y);
    irratehelpers.push_back(rh8y);
    irratehelpers.push_back(rh9y);
    irratehelpers.push_back(rh10y);
    irratehelpers.push_back(rh11y);
    irratehelpers.push_back(rh12y);
    irratehelpers.push_back(rh13y);
    irratehelpers.push_back(rh14y);
    irratehelpers.push_back(rh15y);
    irratehelpers.push_back(rh16y);
    irratehelpers.push_back(rh17y);
    irratehelpers.push_back(rh18y);
    irratehelpers.push_back(rh19y);
    irratehelpers.push_back(rh20y);
    irratehelpers.push_back(rh21y);
    irratehelpers.push_back(rh22y);
    irratehelpers.push_back(rh23y);
    irratehelpers.push_back(rh24y);
    irratehelpers.push_back(rh25y);
    irratehelpers.push_back(rh26y);
    irratehelpers.push_back(rh27y);
    irratehelpers.push_back(rh28y);
    irratehelpers.push_back(rh29y);
    irratehelpers.push_back(rh30y);
    irratehelpers.push_back(rh35y);
    irratehelpers.push_back(rh40y);
    irratehelpers.push_back(rh50y);
    irratehelpers.push_back(rh60y);

    Handle<YieldTermStructure> yts(
        boost::make_shared<PiecewiseYieldCurve<Discount, LogLinear> >(
            0, TARGET(), irratehelpers, Actual365Fixed()));

    // set up the swaption vol cube

    // atm

    Period swapTenorsVal[] = { 1 * Years,  2 * Years,  3 * Years,  4 * Years,
                               5 * Years,  6 * Years,  7 * Years,  8 * Years,
                               9 * Years,  10 * Years, 15 * Years, 20 * Years,
                               30 * Years };

    Period optionTenorsVal[] = {
        1 * Months, 2 * Months,  3 * Months, 6 * Months, 9 * Months,
        1 * Years,  18 * Months, 2 * Years,  3 * Years,  4 * Years,
        5 * Years,  6 * Years,   7 * Years,  8 * Years,  9 * Years,
        10 * Years, 15 * Years,  20 * Years, 25 * Years, 30 * Years
    };

    // put number of atm swap tenors and option tenors here

    Size noSwAtm = 13, noOpAtm = 20;

    // rows are option tenors, columns are swap tenors, values are times 100

    Real swaptionatmvalues[] = {
        117.8,83.5,75.8,63.6,52.9,44.7,39,34.8,31.7,29.3,23.1,21.1,19.8,
        111.4,78.5,73,61.7,52.4,44,38.6,34.7,31.8,29.6,23.2,21.1,20,
        111.9,78.4,71.8,60.4,51.9,44.4,39,35.2,32.3,30.1,23.7,21.5,20.2,
        105.5,76.2,68.2,57.8,50.7,44.2,39.6,36,33.1,30.8,24.5,22.5,21.4,
        102.4,73.3,65.5,56.9,49.2,43.1,38.8,35.4,32.9,30.9,25.1,23.1,21.9,
        99.3,69.4,61.4,54.1,47.3,42.3,38.3,35.2,32.9,30.9,25.4,23.4,22.3,
        91.7,63.6,55.1,48.8,43.6,39.3,36.1,33.8,32,30.4,25.3,23.8,22.9,
        81.4,58.2,50.7,45.1,40.8,37.2,34.5,32.5,30.9,29.7,25.4,24.1,23.3,
        58.9,46.3,41.5,38,35.3,33.1,31.3,29.9,28.8,28,24.9,24,23.4,
        48.1,39.5,36,33.4,31.6,30.1,28.9,28.1,27.3,26.6,24.4,23.8,23.2,
        39.5,34.1,31.6,30,28.6,27.7,27,26.4,26,25.7,24.1,23.6,22.9,
        33.5,29.9,28.2,27.2,26.3,25.7,25.3,25,24.8,24.7,23.4,22.9,22.1,
        29.6,27,25.9,25.2,24.6,24.3,24.1,24,23.9,23.9,22.8,22.3,21.4,
        26.7,24.9,24.2,23.7,23.4,23.2,23.2,23.2,23.2,23.3,22.5,21.9,20.9,
        24.9,23.6,23.1,22.9,22.7,22.7,22.7,22.8,22.9,23,22.1,21.6,20.4,
        23.7,22.7,22.4,22.4,22.4,22.5,22.6,22.7,22.8,23,22,21.4,20.1,
        22.5,22.2,22.3,22.6,22.8,22.8,22.9,23,23.1,22.9,21.3,19.9,18,
        23.1,22.8,22.7,22.9,23,23,23,23,22.9,22.5,20.1,18.2,16.3,
        22.9,22.7,22.8,22.8,22.7,22.3,22.1,21.8,21.5,21,18.2,16.2,14.8,
        22.3,21.3,21.1,20.9,20.5,20,19.7,19.4,19.1,18.7,16.1,14.8,13.3
    };

    std::vector<Period> optionTenors, swapTenors;
    std::vector<std::vector<Handle<Quote> > > swatmquotes;
    for (int i = 0; i < noOpAtm; i++) {
        optionTenors.push_back(optionTenorsVal[i]);
        std::vector<Handle<Quote> > swatmtemp;
        for (int j = 0; j < noSwAtm; j++) {
            if (i == 0)
                swapTenors.push_back(swapTenorsVal[j]);
            swatmtemp.push_back(Handle<Quote>(boost::shared_ptr<Quote>(
                new SimpleQuote(swaptionatmvalues[i * noSwAtm + j] / 100.0))));
        }
        swatmquotes.push_back(swatmtemp);
    }

    Handle<SwaptionVolatilityStructure> swaptionVolAtm(
        boost::make_shared<SwaptionVolatilityMatrix>(
            TARGET(), ModifiedFollowing, optionTenors, swapTenors, swatmquotes,
            Actual365Fixed()));

    // smile

    Period swapTenorsSmileVal[] = { 2 * Years, 5 * Years, 10 * Years,
                                    20 * Years, 30 * Years };
    Period optionTenorsSmileVal[] = { 3 * Months, 1 * Years,  5 * Years,
                                      10 * Years, 20 * Years, 30 * Years };
    Real smileSpreadVal[] = { -0.02,  -0.01, -0.005, -0.0025, 0.00,
                              0.0025, 0.005, 0.01,   0.02 };

    // opt1 swp1
    // opt1 swp2
    // ...
    // values are abs vol spreads times 100
    // 3M2YNS4, 1Y2YNS4 are set to 0 since no RICs available
    // 3M10YNS4 was changed from -29.68 to +29.68

    Real smileValSpread[] = {
        0,0,316.5,9.15,0,2.54,5.68,10.3,15.29,
        3.1,43.31,4.09,0.55,0,0.62,1.6,3.63,6.81,
        29.68,7.47,0.74,-0.21,0,0.85,1.96,8.75,11.01,
        35.7,9.17,2.76,0.87,0,0.03,0.63,4.42,7.31,
        40.83,12.27,4.57,1.82,0,-0.74,-0.58,2.16,5.32,
        0,215.76,22.44,5.85,0,-2.67,-4.08,-5.44,-6.46,
        391.02,16.02,3.38,1.14,0,-0.53,-0.72,-0.63,0.06,
        37.77,6.28,1.98,0.78,0,-0.47,-0.71,-0.8,-0.2,
        21.04,5.36,1.88,0.77,0,-0.49,-0.75,-0.84,0.96,
        25,6.82,2.44,1,0,-0.6,-0.89,-0.82,1.51,
        22.82,4.96,1.78,0.76,0,-0.56,-0.98,-1.49,-1.85,
        14.6,3.86,1.45,0.63,0,-0.48,-0.84,-1.3,-1.62,
        12.67,3.73,1.45,0.64,0,-0.49,-0.87,-1.35,-1.65,
        12.99,3.71,1.41,0.61,0,-0.46,-0.79,-1.17,-1.28,
        13.76,3.85,1.45,0.63,0,-0.47,-0.8,-1.18,-1.28,
        8.53,2.59,1.01,0.45,0,-0.35,-0.63,-0.99,-1.26,
        8.8,2.63,1.01,0.44,0,-0.34,-0.59,-0.9,-1.05,
        10.86,3.41,1.38,0.62,0,-0.5,-0.9,-1.46,-1.97,
        12.07,3.42,1.3,0.56,0,-0.42,-0.73,-1.08,-1.2,
        11.96,3.27,1.22,0.52,0,-0.38,-0.65,-0.95,-0.99,
        14.59,4.1,1.61,0.72,0,-0.58,-1.03,-1.68,-2.32,
        15.46,4.28,1.69,0.76,0,-0.61,-1.1,-1.82,-2.57,
        16.44,4.55,1.83,0.83,0,-0.69,-1.27,-2.15,-3.21,
        13.02,3.57,1.42,0.64,0,-0.52,-0.95,-1.6,-2.32,
        12.21,3.33,1.31,0.59,0,-0.47,-0.85,-1.4,-1.96,
        14.93,3.98,1.55,0.69,0,-0.55,-0.99,-1.61,-2.23,
        15.46,4.09,1.59,0.71,0,-0.56,-1.01,-1.64,-2.24,
        14.44,3.94,1.56,0.7,0,-0.57,-1.03,-1.71,-2.44,
        11.48,3.1,1.21,0.54,0,-0.43,-0.77,-1.25,-1.71,
        10.56,2.86,1.11,0.49,0,-0.38,-0.68,-1.07,-1.38
    };

    // put number of swap tenors, option tenors and strike
    // spreads for smile here

    Size noSwSmile = 5, noOpSmile = 6, noSmileSpreads = 9;

    std::vector<Period> swapTenorsSmile, optionTenorsSmile;
    std::vector<Real> smileSpreads;
    std::vector<std::vector<Handle<Quote> > > swaptionsmilequotes;

    for (int i = 0; i < noSwSmile; i++)
        swapTenorsSmile.push_back(swapTenorsSmileVal[i]);
    for (int i = 0; i < noOpSmile; i++)
        optionTenorsSmile.push_back(optionTenorsSmileVal[i]);
    for (int i = 0; i < noSmileSpreads; i++)
        smileSpreads.push_back(smileSpreadVal[i]);

    for (int i = 0; i < noSwSmile * noOpSmile; i++) {
        std::vector<Handle<Quote> > qSwSmileTmp;
        for (int j = 0; j < noSmileSpreads; j++) {
            qSwSmileTmp.push_back(
                Handle<Quote>(boost::shared_ptr<Quote>(new SimpleQuote(
                    smileValSpread[i * noSmileSpreads + j] / 100.0))));
        }
        swaptionsmilequotes.push_back(qSwSmileTmp);
    }

    boost::shared_ptr<SwapIndex> swapIndex(
        new EuriborSwapIsdaFixA(30 * Years, yts));
    boost::shared_ptr<SwapIndex> shortSwapIndex(
        new EuriborSwapIsdaFixA(1 * Years, yts));

    // we use the linearly interpolated cube, which is enough in this context

    Handle<SwaptionVolatilityStructure> swaptionCube(
        boost::make_shared<SwaptionVolCube2>(
            swaptionVolAtm, optionTenorsSmile, swapTenorsSmile, smileSpreads,
            swaptionsmilequotes, swapIndex, shortSwapIndex, false));

    // times, accruals, forwards setup

    // we need dealt maturity plus 10y for the cms (minus 6m which we ignore
    // here though)

    Date termDate = Date(29, July, 2056);

    Schedule forwardSched(settlDate, termDate, 6 * Months, TARGET(), Following,
                          Following, DateGeneration::Backward, false);

    std::vector<Real> rateTimes(forwardSched.size());

    // daycount w.r.t. which forwards are expressed (the term structures day
    // counter actually)

    DayCounter forwardsDc = Actual365Fixed();
    for (Size i = 0; i < forwardSched.size(); ++i) {
        rateTimes[i] = forwardsDc.yearFraction(evalDate, forwardSched[i]);
    }

    std::cout << "Forward Schedule (evaluation date is " << evalDate << ")"
              << std::endl;
    std::cout << "index;start;end;forward" << std::endl;

    std::vector<Real> todaysForwards(rateTimes.size() - 1);
    std::vector<Real> displacements(rateTimes.size() - 1);
    std::vector<Real> volatilities(rateTimes.size() - 1);
    for (int i = 0; i < todaysForwards.size(); i++) {
        todaysForwards[i] =
            yts->forwardRate(rateTimes[i], rateTimes[i + 1], Simple, Annual);
        displacements[i] = 0.10; // put displacements here (just dummy values,
                                 // will be overwritten later)
        volatilities[i] = 1.0;   // put vol adjustments here (just dummy values,
                                 // will be overwritten later)
        std::cout << i << ";" << forwardSched.date(i) << ";"
                  << forwardSched.date(i + 1) << ";" << todaysForwards[i]
                  << std::endl;
    }

    // calibration error evaluation

    struct CalibrationError {

        CalibrationError(const Handle<YieldTermStructure> &yts,
                         const Handle<SwaptionVolatilityStructure> &vcube,
                         const std::vector<Real> &rateTimes,
                         const std::vector<Real> &todaysForwards)
            : yts_(yts), vcube_(vcube), rateTimes_(rateTimes),
              todaysForwards_(todaysForwards) {

            // swap fixed leg is covering step periods of forwards rates

            const Size step = 2;

            // calibration basket definition

            // on cms fixings we calibrate on the 10y smile
            // with spreads -200, -100, 0, 100, 200 (leaving out -50, -25, 25,
            // 50)

            const Size swpFixIdx[] = {
                39, 39, 39, 39, 39, 40, 40, 40, 40, 40, 41, 41, 41, 41, 41,
                42, 42, 42, 42, 42, 43, 43, 43, 43, 43, 44, 44, 44, 44, 44,
                45, 45, 45, 45, 45, 46, 46, 46, 46, 46, 47, 47, 47, 47, 47,
                48, 48, 48, 48, 48, 49, 49, 49, 49, 49, 50, 50, 50, 50, 50,
                51, 51, 51, 51, 51, 52, 52, 52, 52, 52, 53, 53, 53, 53, 53,
                54, 54, 54, 54, 54, 55, 55, 55, 55, 55, 56, 56, 56, 56, 56,
                57, 57, 57, 57, 57, 58, 58, 58, 58, 58, 59, 59, 59, 59, 59,
                60, 60, 60, 60, 60, 61, 61, 61, 61, 61, 62, 62, 62, 62, 62,
                63, 63, 63, 63, 63, 64, 64, 64, 64, 64
                // , 39, 39, 39, 39, 39 // coterminal
            };

            const Size swpEndIdx[] = {
                59, 59, 59, 59, 59, 60, 60, 60, 60, 60, 61, 61, 61, 61, 61,
                62, 62, 62, 62, 62, 63, 63, 63, 63, 63, 64, 64, 64, 64, 64,
                65, 65, 65, 65, 65, 66, 66, 66, 66, 66, 67, 67, 67, 67, 67,
                68, 68, 68, 68, 68, 69, 69, 69, 69, 69, 70, 70, 70, 70, 70,
                71, 71, 71, 71, 71, 72, 72, 72, 72, 72, 73, 73, 73, 73, 73,
                74, 74, 74, 74, 74, 75, 75, 75, 75, 75, 76, 76, 76, 76, 76,
                77, 77, 77, 77, 77, 78, 78, 78, 78, 78, 79, 79, 79, 79, 79,
                80, 80, 80, 80, 80, 81, 81, 81, 81, 81, 82, 82, 82, 82, 82,
                83, 83, 83, 83, 83, 84, 84, 84, 84, 84
                // , 84, 84, 84, 84, 84 // coterminal
            };

            const Real atmOffset[] = {
                -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02,
                -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02,
                -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02,
                -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02,
                -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02,
                -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02,
                -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02,
                -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02,
                -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02,
                -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02,
                -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02,
                -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02,
                -0.02, -0.01, 0.0, 0.01, 0.02, -0.02, -0.01, 0.0, 0.01, 0.02,
                // -0.02, -0.01, 0.0, 0.01, 0.02 // coterminal
            };

            Size noCalInstr = 26 * 5;// +5; // the last 5 are coterminals

            LMMCurveState curve0(rateTimes);
            curve0.setOnForwardRates(todaysForwards);
            Real d0 = yts->discount(rateTimes[0]);

            for (Size i = 0; i < noCalInstr; i++) {
                Real strike =
                    curve0.swapRate(swpFixIdx[i], swpEndIdx[i], step) +
                    atmOffset[i];
                if (strike > 0.0010) { // only take swaptions with strike > 10bp
                    strikes_.push_back(strike);
                    forwards_.push_back(
                        curve0.swapRate(swpFixIdx[i], swpEndIdx[i], step));
                    annuities_.push_back(
                        d0 * curve0.swapAnnuity(0, swpFixIdx[i], swpEndIdx[i],
                                                step));
                    fixingTimes_.push_back(rateTimes[swpFixIdx[i]]);
                    boost::shared_ptr<StrikedTypePayoff> payoff(
                        new PlainVanillaPayoff(Option::Call, strikes_[i]));
                    MultiStepSwaption calInstr(rateTimes, swpFixIdx[i],
                                               swpEndIdx[i], payoff, step);
                    marketVols_.push_back(vcube_->volatility(
                        fixingTimes_[i], (swpEndIdx[i] - swpFixIdx[i]) / 2.0,
                        strikes_[i], true));
                    marketPrices_.push_back(blackFormula(
                        Option::Call, strikes_[i], forwards_[i],
                        sqrt(fixingTimes_[i]) * marketVols_[i], annuities_[i]));
                    weights_.push_back(1.0);
                    calibrationInstruments_.add(calInstr);
                }
            }

            calibrationInstruments_.finalize();

            // evolution

            evolution_ = calibrationInstruments_.evolution();

            // numeraires

            numeraires_ = terminalMeasure(evolution_);

            // correlation (is fixed!)

            Matrix corrMatrix =
                exponentialCorrelations(rateTimes, LTCORR, CORRDECAY);
            corr_ = boost::shared_ptr<PiecewiseConstantCorrelation>(
                new TimeHomogeneousForwardCorrelation(corrMatrix,
                                                      evolution_.rateTimes()));
            // rescale weights so that sum of sqaure is 1.0

            Real sumw = 0.0;
            for (int i = 0; i < weights_.size(); i++)
                sumw += weights_[i] * weights_[i];
            for (int i = 0; i < weights_.size(); i++)
                weights_[i] /= sqrt(sumw);

        } // end CalibrationError constructor

        Disposable<Array>
        operator()(const std::vector<Real> &displacements,
                   const std::vector<Real> &volatilities, const Real a,
                   const Real b, const Real c, const Real d, const Real volvol,
                   const Real meanRev,
                   const Size paths = CALIBRATIONPATHS) const {

            boost::shared_ptr<MarketModel> model(
                new AbcdVol(a, b, c, d, volatilities, corr_, evolution_,
                            FACTORS, todaysForwards_, displacements));

            long seed = SEED;

            // Choose you RNG factory here
            
            boost::shared_ptr<BrownianGeneratorFactory> generatorFactory;
            if(sobol) {
                generatorFactory = boost::make_shared<SobolBrownianGeneratorFactory>(SobolBrownianGenerator::Diagonal, seed);
            }
            else {
                generatorFactory = boost::make_shared<MTBrownianGeneratorFactory>(seed);
            }

            boost::shared_ptr<MarketModelEvolver> evolver;

            if(!stochvol) {

                // SBGM

                evolver = boost::shared_ptr<MarketModelEvolver>(
                    // what does initial step mean?
                    new LogNormalFwdRatePc(model, *generatorFactory, numeraires_,
                                           0));

            } else {

                // FLSV

                // fancy parameters here ?

                boost::shared_ptr<MarketModelVolProcess> volProcess(
                    new SquareRootAndersen(1.0, meanRev, volvol, 1.0,
                                           evolution_.evolutionTimes(), 8, 0.5,
                                           0.5, 1.5));

                evolver = boost::shared_ptr<MarketModelEvolver>(
                    new SVDDFwdRatePc(model, *generatorFactory, volProcess, 2, 2,
                                      numeraires_));
            }

            Size initialNumeraire = evolver->numeraires().front();

            Real initialNumeraireValue =
                yts_->discount(rateTimes_[initialNumeraire]);

            AccountingEngine engine(evolver, calibrationInstruments_,
                                    initialNumeraireValue);

            boost::shared_ptr<SequenceStatisticsInc> stats(
                new SequenceStatisticsInc(
                    calibrationInstruments_.numberOfProducts()));

            engine.multiplePathValues(*stats, paths);

            Array result(calibrationInstruments_.size());

            std::cout << "#;fixing;strike;pricemk;pricemd;volmk;volmd;weight;error(vega)"
                      << std::endl;
            std::cout << std::setprecision(5)
                      << std::setiosflags(std::ios::fixed);
            for (Size i = 0; i < calibrationInstruments_.size(); i++) {
                Real modelp = stats->mean()[i]; // model price
                Real modelv = 0.0;              // implied vol
                try {
                    modelv = blackFormulaImpliedStdDev(
                                 Option::Call, strikes_[i], forwards_[i],
                                 modelp, annuities_[i]) /
                             sqrt(fixingTimes_[i]);
                }
                catch (...) {
                }
                Real marketp = marketPrices_[i]; // market price
                Real marketv = marketVols_[i];   // market vol
                result[i] =
                    weights_[i] *
                    (modelp -
                     marketp); // measure error in terms of price difference
                std::cout << i << ";" << fixingTimes_[i] << ";" << strikes_[i]
                          << ";" << marketp << ";" << modelp << ";" << marketv
                          << ";" << modelv << ";" << weights_[i] << ";" << 100.0*(marketv-modelv) << std::endl;
            }

            return result;
        }

        const Handle<YieldTermStructure> &yts_;
        const Handle<SwaptionVolatilityStructure> &vcube_;
        const std::vector<Real> &rateTimes_;
        const std::vector<Real> &todaysForwards_;
        MultiProductComposite calibrationInstruments_;
        EvolutionDescription evolution_;
        std::vector<Real> marketPrices_, marketVols_, strikes_, forwards_,
            annuities_, fixingTimes_, weights_;
        std::vector<Size> numeraires_;
        boost::shared_ptr<PiecewiseConstantCorrelation> corr_;

    }; // end CalibrationError

    // calibration

    CalibrationError calErr(yts, swaptionCube, rateTimes, todaysForwards);

    // this is taken from ABCD calibration

    class AbcdParametersTransformation : public ParametersTransformation {
        mutable Array y_;
        const Real eps1_;

      public:
        AbcdParametersTransformation() : y_(Array(4)), eps1_(.000000001) {}

        Array direct(const Array &x) const {
            y_[0] = x[0] * x[0] - x[3] * x[3] + eps1_; // a + d > 0
            y_[1] = x[1];
            y_[2] = x[2] * x[2] + eps1_; // c > 0
            y_[3] = x[3] * x[3] + eps1_; // d > 0
            return y_;
        }

        Array inverse(const Array &x) const {
            y_[0] = std::sqrt(x[0] + x[3] - eps1_);
            y_[1] = x[1];
            y_[2] = std::sqrt(x[2] - eps1_);
            y_[3] = std::sqrt(x[3] - eps1_);
            return y_;
        }
    };

    // cost function to calibrate abcd and constant displacement for all rates
    // vector x = (a',b',c',d', d0', dinf', dl', v0', vinf', vl', volvol',
    // meanRev')
    // where a',b',c',d' are transformed via AbcdParametersTransformation and
    // d0', dinf', dl', v0', vinf', vl', volvol', meanRev' are transformed via
    // y=x^2
    // and displacements = dinf + (d0-dinf) * exp(-dl * #libor)
    // and volatilities = vinf + (v0-vinf) * exp(-vl * #libor)

    class CalAbcdDis : public CostFunction {

      public:
        CalAbcdDis(CalibrationError *calError, Size numberOfForwards)
            : calError_(calError), numberOfForwards_(numberOfForwards) {}

        Real value(const Array &x) const {
            Array z = values(x);
            Real res = 0.0;
            for (int i = 0; i < z.size(); i++) {
                res += z[i] * z[i];
            }
            // res/=z.size(); // done via weights
            return sqrt(res);
        }

        Disposable<Array> values(const Array &x) const {
            std::vector<Real> displacements(numberOfForwards_);
            std::vector<Real> volatilities(numberOfForwards_);
            AbcdParametersTransformation trans;
            Array y = trans.direct(x);
            Real d0, dinf, dl, v0, vinf, vl, volvol, meanrev;
            d0 = x[4] * x[4];
            dinf = x[5] * x[5];
            dl = x[6] * x[6];
            v0 = x[7] * x[7];
            vinf = x[8] * x[8];
            vl = x[9] * x[9];
            d0 = std::min(d0, 1.0);
            dinf = std::min(dinf, 1.0);
            v0 = std::min(v0, 2.0);
            vinf = std::min(vinf, 2.0);
            volvol = x[10]*x[10];   /// FIX THE VOLVOL if you want here ...
            meanrev = x[11] * x[11];
            for (int i = 0; i < numberOfForwards_; i++) {
                Real idx = (double)i / (double)numberOfForwards_;
                displacements[i] = dinf + exp(-idx * dl) * (d0 - dinf);
                volatilities[i] = vinf + exp(-idx * vl) * (v0 - vinf);
            }
            std::cout << "Trying (a,b,c,d)=(" << y[0] << "," << y[1] << ","
                      << y[2] << "," << y[3] << ")" << std::endl;
            std::cout << "(d0,dinf,dl)=(" << d0 << "," << dinf << "," << dl
                      << "), (v0,vinf,vl)=(" << v0 << "," << vinf << "," << vl
                      << ")" << std::endl;
            std::cout << "(volvol,meanrev)=(" << volvol << "," << meanrev << ")"
                      << std::endl;
            std::cout << "Displacements: ";
            for (int i = 0; i < numberOfForwards_; i++)
                std::cout << displacements[i] << ",";
            std::cout << std::endl;
            std::cout << "Phis: ";
            for (int i = 0; i < numberOfForwards_; i++)
                std::cout << volatilities[i] << ",";
            std::cout << std::endl;
            Array error =
                calError_->operator()(displacements, volatilities, y[0], y[1],
                                      y[2], y[3], volvol, meanrev);
            Real sumErr = 0.0;
            for (Size i = 0; i < error.size(); i++) {
                sumErr += error[i] * error[i];
            }
            // sumErr/=error.size(); // done via weights
            std::cout << "Error: " << sqrt(sumErr) << std::endl;
            std::cout << "-----------------------------------------------------"
                      << "-----------------------------------------------------"
                      << std::endl;
            return error;
        }

      private:
        CalibrationError *calError_;
        Size numberOfForwards_;

    }; // end CalAbcdDis

    // Calibrate the model !

    if (calibrate) {
        std::cout << "CALIBRATION STARTED" << std::endl;
        CalAbcdDis calCost(&calErr, todaysForwards.size());
        NoConstraint noConstraint;
        AbcdParametersTransformation trans;
        // START VALUES CALIBRATION abcd
        Array startAbcd(4);
        startAbcd[0] = 0.05207;
        startAbcd[1] = -0.11282;
        startAbcd[2] = 0.66262;
        startAbcd[3] = 0.03728;
        Array startAbcdInv = trans.inverse(startAbcd);
        Array start(
            4 + 3 + 3 +
            2); // 4 abcd, 3 displacement, 3 voladjusterrs, 2 volvol,meanrev
        start[0] = startAbcdInv[0];
        start[1] = startAbcdInv[1];
        start[2] = startAbcdInv[2];
        start[3] = startAbcdInv[3];
        // START VALUES transformed d0, dinf, dl, v0, vinf, vl
        start[4] = sqrt(0.30922);
        start[5] = sqrt(0.04183);
        start[6] = sqrt(3.83934);
        start[7] = sqrt(1.46095);
        start[8] = sqrt(2.02632);
        start[9] = sqrt(5.37381);
        // START VALUES volvol, meanrev
        start[10] = sqrt(1.25);
        start[11] = sqrt(0.15);
        Problem calProblem(calCost, noConstraint, start);
        EndCriteria endC(2500, 10, 1E-14, 1E-5, 1E-5);
        LevenbergMarquardt opt;
        //Simplex opt(0.01);
        opt.minimize(calProblem, endC);
        // set calibrated values
        Array result = calProblem.currentValue();
        Array dres = trans.direct(result);
        Real a = dres[0];
        Real b = dres[1];
        Real c = dres[2];
        Real d = dres[3];
        Real d0 = result[4] * result[4];
        Real dinf = result[5] * result[5];
        Real dl = result[6] * result[6];
        Real v0 = result[7] * result[7];
        Real vinf = result[8] * result[8];
        Real vl = result[9] * result[9];
        Real volvol = result[10] * result[10];
        Real meanrev = result[11] * result[11];
        std::cout << "Calibration result:" << std::endl;
        std::cout << "a,b,c,d = " << a << "," << b << "," << c << "," << d
                  << std::endl;
        std::cout << "d0,dinf,dl = " << d0 << "," << dinf << "," << dl
                  << std::endl;
        std::cout << "v0,vinf,vl = " << v0 << "," << vinf << "," << vl
                  << std::endl;
        std::cout << "volvol, meanrev = " << volvol << "," << meanrev
                  << std::endl;
    }

    // Price the OTC deal !

    if (price) {
        std::cout << "PRICING STARTED" << std::endl;

        // stochvol always false

        // ----------------------
        // CORR 0.15 0.25 (?%) ?m
        // ----------------------
        // Real a = 0.04972;
        // Real b = -0.12367;
        // Real c = 0.71036;
        // Real d = 0.03916;
        // // Real d=0.0389232; // shifted d for vega calculation
        // Real d0 = 0.31702;
        // Real dinf = 0.03823;
        // Real dl = 3.71784;
        // Real v0 = 1.46394;
        // Real vinf = 2.0;
        // Real vl = 15.34620;
        // Real volvol = 1.27709;
        // Real meanrev = 0.18530;
        
        // ----------------------
        // CORR 0.2 0.1 (74%) 10m
        // ----------------------
        Real a = -0.00173;
        Real b = -0.11219;
        Real c = 0.65132;
        Real d = 0.03770;
        // Real d=0.0389232; // shifted d for vega calculation
        Real d0 = 0.26822;
        Real dinf = 0.02781;
        Real dl = 3.00837;
        Real v0 = 2.0;
        Real vinf = 2.0;
        Real vl = 1.0;
        Real volvol = 1.27709;
        Real meanrev = 0.18530;

        // ----------------------
        // CORR 0.2 0.05 (77%) 8.8m
        // ----------------------
        // Real a = -0.02051;
        // Real b = -0.11897;
        // Real c = 0.66997;
        // Real d = 0.03684;
        // // Real d=0.0389232; // shifted d for vega calculation
        // Real d0 = 0.33043;
        // Real dinf = 0.03636;
        // Real dl = 3.87075;
        // Real v0 =2.0;
        // Real vinf = 2.0;
        // Real vl = 1.0;
        // Real volvol = 1.27709;
        // Real meanrev = 0.18530;

        // ----------------------
        // CORR 0.90 0.05 (91%) 3.7m
        // ----------------------
        // Real a = 0.03440;
        // Real b = -0.10630;
        // Real c = 0.69412;
        // Real d = 0.03705;
        // // Real d=0.0389232; // shifted d for vega calculation
        // Real d0 = 0.20332;
        // Real dinf = 0.03442;
        // Real dl = 3.57596;
        // Real v0 =2.0;
        // Real vinf = 2.0;
        // Real vl = 1.0;
        // Real volvol = 1.27709;
        // Real meanrev = 0.18530;

        // ----------------------
        // CORR 0.99 0.01 (97%) 1.7m
        // ----------------------
        // Real a = -0.00727;
        // Real b = -0.09966;
        // Real c = 1.07715;
        // Real d = 0.03514;
        // // Real d=0.0389232; // shifted d for vega calculation
        // Real d0 = 0.21127;
        // Real dinf = 0.03700;
        // Real dl = 3.92970;
        // Real v0 =2.0;
        // Real vinf = 2.0;
        // Real vl = 1.0;
        // Real volvol = 1.27709;
        // Real meanrev = 0.18530;

        for (int i = 0; i < todaysForwards.size(); i++) {
            Real idx = (double)i / (double)todaysForwards.size();
            displacements[i] = dinf + exp(-idx * dl) * (d0 - dinf);
            volatilities[i] = vinf + exp(-idx * vl) * (v0 - vinf);
        }

        // check calibration with #paths as in pricing
        calErr(displacements, volatilities, a, b, c, d, volvol, meanrev,
               PRICINGPATHS);

        // check the match of the CMS01 quotes for 20y and 30y
        Schedule cms20ySched(settlDate, TARGET().advance(settlDate,20*Years), 6*Months,TARGET(),ModifiedFollowing,ModifiedFollowing,DateGeneration::Backward,false);
        Schedule cms30ySched(settlDate, TARGET().advance(settlDate,30*Years), 6*Months,TARGET(),ModifiedFollowing,ModifiedFollowing,DateGeneration::Backward,false);
        std::vector<Real> cms20yAccr;
        std::vector<Real> cms30yAccr;
        std::vector<Real> payTimes20y;
        std::vector<Real> payTimes30y;
        Real annuity20y = 0.0, annuity30y = 0.0;
        for(Size i=0;i<cms20ySched.size()-1;i++) {
            cms20yAccr.push_back(Actual360().yearFraction(cms20ySched.date(i),cms20ySched.date(i+1)));
            payTimes20y.push_back(yts->timeFromReference(cms20ySched.date(i+1)));
            annuity20y += cms20yAccr[i] * yts->discount(cms20ySched.date(i+1));
        }
        for(Size i=0;i<cms30ySched.size()-1;i++) {
            cms30yAccr.push_back(Actual360().yearFraction(cms30ySched.date(i),cms30ySched.date(i+1)));
            payTimes30y.push_back(yts->timeFromReference(cms30ySched.date(i+1)));
            annuity30y += cms30yAccr[i] * yts->discount(cms30ySched.date(i+1));
        }
        for(Size i=cms20yAccr.size(); i <= rateTimes.size(); i++) {
            cms20yAccr.push_back(0.0);
            payTimes20y.push_back(100.0);
        }
        for(Size i=cms30yAccr.size(); i <= rateTimes.size(); i++) {
            cms30yAccr.push_back(0.0);
            payTimes30y.push_back(100.0);
        }
        Real annuity2030y = annuity30y - annuity20y;

        MultiStepCmsSwap cms20y(rateTimes, cms20yAccr, cms20yAccr, payTimes20y, 20, 2, 0.0, 0.0, Null<Real>(), Null<Real>(), false);
        MultiStepCmsSwap cms30y(rateTimes, cms30yAccr, cms30yAccr, payTimes30y, 20, 2, 0.0, 0.0, Null<Real>(), Null<Real>(), false);

        // set up underlying cms swap

        Schedule paySched(Date(29,July,2033),Date(29,July,2046),6*Months,TARGET(),Unadjusted,Unadjusted,DateGeneration::Backward,false);
        Schedule recSched(Date(29,July,2033),Date(29,July,2046),6*Months,TARGET(),ModifiedFollowing,ModifiedFollowing,DateGeneration::Backward,false);

        // set the coupons until underlying swap start to zero
        std::vector<Real> cmsAccruals(39,0.0);
        std::vector<Real> floatAccruals(39,0.0); 
        std::vector<Real> paymentTimes(39,100.0); // doesnt matter

        std::cout << "Deal schedules" << std::endl;
        std::cout << "payStart;payEnd;recStart;recEnd" << std::endl;
        for(Size i=0;i<paySched.size()-1;i++) {
            std::cout << paySched.date(i) << ";" << paySched.date(i+1) << ";"
                      << recSched.date(i) << ";" << recSched.date(i+1) << std::endl;
            cmsAccruals.push_back(Thirty360().yearFraction(paySched.date(i),paySched.date(i+1)));
            floatAccruals.push_back(Actual365Fixed().yearFraction(recSched.date(i),recSched.date(i+1))); // this is more consistent to the estimated libor rate
            paymentTimes.push_back(yts->timeFromReference(paySched.date(i+1)));
        }

        for(Size i=cmsAccruals.size(); i <= rateTimes.size(); i++) {
            cmsAccruals.push_back(0.0);
            floatAccruals.push_back(0.0);
            paymentTimes.push_back(100.0);
        }

        // todo - libor rate Act/360 vs Act/365, cms rate 30/360 vs Act/365
        
        // we apply two corrections, 1 for basis (16 for cms01 quote matching ? => no)
        MultiStepCmsSwap underlying(rateTimes, cmsAccruals, floatAccruals,
                                    paymentTimes, 20, 2, 0.0005 /*+ 0.0016*/, 0.0035-0.0001,
                                    Null<Real>(), 0.09, true);
        MultiStepCmsSwap underlyingRev(rateTimes, cmsAccruals, floatAccruals,
                                       paymentTimes, 20, 2, 0.0005 /*+ 0.0016*/, 0.0035-0.0001,
                                    Null<Real>(), 0.09, false);

        //exercise schedule
        std::vector<Rate> exerciseTimes;
        exerciseTimes.push_back(rateTimes[39]);

        EvolutionDescription evolution = underlying.evolution();
		std::vector<Size> numeraires = terminalMeasure(evolution);
		// correlation (is fixed!)
		Matrix corrMatrix = exponentialCorrelations(rateTimes,LTCORR,CORRDECAY);
        // std::cout << "Correlation Matrix: " << std::endl << std::setprecision(2);
        // for(Size i=0;i<corrMatrix.rows();i++) {
        //     for(Size j=0;j<corrMatrix.columns();j++) {
        //         std::cout << corrMatrix[i][j] << ";";
        //     }
        //     std::cout << std::endl;
        // }
        // std::cout << std::endl;

		boost::shared_ptr<PiecewiseConstantCorrelation> corr(new TimeHomogeneousForwardCorrelation(corrMatrix,evolution.rateTimes()));
		boost::shared_ptr<MarketModel> model(new AbcdVol(a,b,c,d,volatilities,corr,evolution,FACTORS,todaysForwards,displacements));

		long seed = SEED;
		Size paths = PRICINGPATHS;
        boost::shared_ptr<BrownianGeneratorFactory> generatorFactory;
        if(sobol) {
            generatorFactory = boost::make_shared<SobolBrownianGeneratorFactory>(SobolBrownianGenerator::Diagonal, seed);
        }
        else {
            generatorFactory = boost::make_shared<MTBrownianGeneratorFactory>(seed);
        }

        boost::shared_ptr<MarketModelEvolver> evolver;

        if(!stochvol) {
            evolver = boost::make_shared<LogNormalFwdRatePc>(model, *generatorFactory,numeraires,0); 
            // boost::shared_ptr<MarketModelEvolver> evolver(new LogNormalFwdRateEuler(model, generatorFactory,numeraires,0)); 
        }
        else {
            boost::shared_ptr<MarketModelVolProcess> volProcess(new SquareRootAndersen(1.0,meanrev,volvol,1.0,evolution.evolutionTimes(),8,0.5,0.5,1.5)); // last 4 parameters here ?
            evolver = boost::make_shared<SVDDFwdRatePc>(model, *generatorFactory, volProcess, 2, 2, numeraires); // what does 2, 2 mean?
        }

		Size initialNumeraire = evolver->numeraires().front();
		Real initialNumeraireValue = yts->discount(rateTimes[initialNumeraire]);

        //debug
        LMMCurveState state(rateTimes);
        state.setOnForwardRates(todaysForwards);
        std::cout << "initial fair rate " << state.swapRate(39,65,2) << std::endl;
        //end debug

        // calculate the exercise strategy
        // Longstaff-Schwartz exercise strategy
        std::valarray<bool> isExercise(false,rateTimes.size()-1);
        isExercise[39]=true;
        std::vector<std::vector<NodeData> > collectedData;
        std::vector<std::vector<Real> > basisCoefficients;
        NothingExerciseValue control(rateTimes,isExercise);
        //basis: forward and coterminal swap rate
        std::vector<boost::function<Size(Size)> > start, end;
        boost::function<Size(Size)> s = boost::lambda::constant(39);  // 
        boost::function<Size(Size)> e1 = boost::lambda::constant(40); // 6m fwd
        // boost::function<Size(Size)> e2 = boost::lambda::constant(41); // 
        // boost::function<Size(Size)> e3 = boost::lambda::constant(43); // 
        // boost::function<Size(Size)> e4 = boost::lambda::constant(45); // 
        // boost::function<Size(Size)> e5 = boost::lambda::constant(47); // 
        boost::function<Size(Size)> e6 = boost::lambda::constant(49); // 5y cms rate
        // boost::function<Size(Size)> e7 = boost::lambda::constant(51); // 
        // boost::function<Size(Size)> e8 = boost::lambda::constant(53); // 
        // boost::function<Size(Size)> e9 = boost::lambda::constant(55); // 
        // boost::function<Size(Size)> e10 = boost::lambda::constant(57); // 
        boost::function<Size(Size)> e11 = boost::lambda::constant(59); // 10 cm rate
        // boost::function<Size(Size)> e12 = boost::lambda::constant(61); // 
        // boost::function<Size(Size)> e13 = boost::lambda::constant(63); // 
        // boost::function<Size(Size)> e14 = boost::lambda::constant(65); // 
        // boost::function<Size(Size)> e15 = boost::lambda::constant(67); // 
        boost::function<Size(Size)> e16 = boost::lambda::constant(69); // 15y cm rate
        // boost::function<Size(Size)> e17 = boost::lambda::constant(71); // 
        // boost::function<Size(Size)> e18 = boost::lambda::constant(72); // 
        // boost::function<Size(Size)> e19 = boost::lambda::constant(73); // 
        // boost::function<Size(Size)> e20 = boost::lambda::constant(74); // 
        boost::function<Size(Size)> e21 = boost::lambda::constant(76); // 20y cm rate
        start.push_back(s); end.push_back(e1);
        // start.push_back(s); end.push_back(e2); 
        // start.push_back(s); end.push_back(e3);
        // start.push_back(s); end.push_back(e4);
        // start.push_back(s); end.push_back(e5);
        start.push_back(s); end.push_back(e6);
        // start.push_back(s); end.push_back(e7);
        //start.push_back(s); end.push_back(e8);
        // start.push_back(s); end.push_back(e9);
        // start.push_back(s); end.push_back(e10);
        start.push_back(s); end.push_back(e11);
        // start.push_back(s); end.push_back(e12);
        //start.push_back(s); end.push_back(e13);
        // start.push_back(s); end.push_back(e14);
        // start.push_back(s); end.push_back(e15);
        start.push_back(s); end.push_back(e16);
        // start.push_back(s); end.push_back(e17);
        //start.push_back(s); end.push_back(e18);
        // start.push_back(s); end.push_back(e19);
        // start.push_back(s); end.push_back(e20);
        start.push_back(s); end.push_back(e21);
        CustomBasisSystem basisSystem(rateTimes,exerciseTimes,start,end);
        NothingExerciseValue nullRebate(rateTimes,isExercise);
        MultiStepNothing nullProduct(evolution);

        collectNodeData(*evolver,underlyingRev,basisSystem,nullRebate,control,TRAININGPATHS,collectedData);

        Real npv0 = genericLongstaffSchwartzRegression(collectedData,
                                                       basisCoefficients) * initialNumeraireValue;

        for(Size i=0;i<basisCoefficients.size();i++) {
            for(Size j=0;j<basisCoefficients[i].size();j++) {
                std::cout << basisCoefficients[i][j] << ";";
            }
            std::cout << std::endl;
        }

        std::cout << "correlation between forward and cms10y = " << correlation(collectedData) << std::endl;

        LongstaffSchwartzExerciseStrategy exerciseStrategy(basisSystem, basisCoefficients, evolution, numeraires, nullRebate, control);

        // std::cout << "Longstaff Schwartz strategy:" << std::endl;
        // std::cout << "Exercise times:" << std::endl;
        // for(Size i=0;i<exerciseStrategy.exerciseTimes().size();i++) std::cout << exerciseStrategy.exerciseTimes()[i] << ";";
        // std::cout << std::endl;
        // std::cout << "Relevant times:" << std::endl;
        // for(Size i=0;i<exerciseStrategy.relevantTimes().size();i++) std::cout << exerciseStrategy.relevantTimes()[i] << ";";
        // std::cout << std::endl;
        
        boost::shared_ptr<StrikedTypePayoff> payoff = boost::make_shared<PlainVanillaPayoff>(Option::Call, 0.0266);
        MultiStepSwaption swaptionRef(rateTimes,39,65,payoff,2);


        CallSpecifiedMultiProduct swaption(nullProduct,exerciseStrategy,underlying);
        CallSpecifiedMultiProduct callable(underlyingRev,exerciseStrategy,ExerciseAdapter(nullRebate));

        MultiProductComposite products;
        products.add(underlyingRev);
        products.add(callable);
        products.add(swaption);
        products.add(cms20y);
        products.add(cms30y);
        products.add(swaptionRef);
        products.finalize();

        AccountingEngine engine(evolver, products, initialNumeraireValue);
        boost::shared_ptr<SequenceStatisticsInc> stats = boost::make_shared<SequenceStatisticsInc>(products.numberOfProducts());

        engine.multiplePathValues(*stats, PRICINGPATHS);

        Real npv = stats->mean()[0];
        Real npv2 = stats->mean()[1];
        Real npv3 = stats->mean()[2];
        Real npv4 = stats->mean()[3];
        Real npv5 = stats->mean()[4];
        Real npv6 = stats->mean()[5];

        Real margin20y = npv4 / annuity20y;
        Real margin30y = npv5 / annuity30y;
        Real margin2030y = (margin30y-margin20y)*annuity20y/annuity2030y + margin20y; 
        
        std::cout << std::setprecision(6) << "npv0 = " << npv0 << std::endl;
        std::cout << "npv(underlying) = " << npv << " npv(callable) = " << npv2 << " npv(option) = " << npv2-npv << std::endl;
        std::cout << "npv(option) = " << npv3 << " = " << npv3*400000000.0 << std::endl;
        std::cout << "npv(cms20y) = " << npv4 << " fair margin " << margin20y*10000.0 << std::endl;
        std::cout << "npv(cms30y) = " << npv5 << " fair margin " << margin30y*10000.0 << std::endl;
        std::cout << "fair 20-30 forward margin = " << margin2030y * 10000.0 << " while market forward margin is " <<
            (78-92.9)*annuity20y/annuity2030y + 92.9 << std::endl;
        std::cout << "swaptionRef = " << npv6 << std::endl;
        
    }

    return 0;

}


