#include <ql/quantlib.hpp>
#include <iostream>

#include <boost/assign/std/vector.hpp>
#include <boost/timer/timer.hpp>

using namespace QuantLib;

using namespace boost::assign;

int main(int,char**) {
    
    boost::timer::auto_cpu_timer t;
    
    std::cout << "VolCube calibration to cms market (04-02-2014)" << std::endl;


    // set evalulation date

    Date evalDate = Date(4, February, 2014);
    Date settlDate = TARGET().advance(evalDate, 2 * Days);
    Settings::instance().evaluationDate() = evalDate;

    // set up the interest rate curves (eoniaBt, 6m forward curve)

    Real eoniaquotes[] = { 0.0013,  0.00156, 0.00163, 0.0016,  0.00163, 0.00147,
                           0.00139, 0.00131, 0.00125, 0.0012,  0.00117, 0.00114,
                           0.00112, 0.0011,  0.00111, 0.0011,  0.00111, 0.00117,
                           0.00127, 0.00143, 0.00264, 0.0045,  0.00656, 0.00866,
                           0.01065, 0.01247, 0.01411, 0.01558, 0.01687, 0.01799,
                           0.02051, 0.02238, 0.02302, 0.02322, 0.02322, 0.02322 };

    boost::shared_ptr<RateHelper> rhon = boost::make_shared<DepositRateHelper>(
        eoniaquotes[0], 1 * Days, 0, TARGET(), Following, false, Actual360());

    boost::shared_ptr<OvernightIndex> eoniaBt = boost::make_shared<Eonia>();

    boost::shared_ptr<RateHelper> rhe1w = boost::make_shared<OISRateHelper>(
        2, 1 * Weeks,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[1])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe2w = boost::make_shared<OISRateHelper>(
        2, 2 * Weeks,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[2])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe3w = boost::make_shared<OISRateHelper>(
        2, 3 * Weeks,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[3])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe1m = boost::make_shared<OISRateHelper>(
        2, 1 * Months,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[4])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe2m = boost::make_shared<OISRateHelper>(
        2, 2 * Months,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[5])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe3m = boost::make_shared<OISRateHelper>(
        2, 3 * Months,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[6])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe4m = boost::make_shared<OISRateHelper>(
        2, 4 * Months,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[7])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe5m = boost::make_shared<OISRateHelper>(
        2, 5 * Months,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[8])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe6m = boost::make_shared<OISRateHelper>(
        2, 6 * Months,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[9])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe7m = boost::make_shared<OISRateHelper>(
        2, 7 * Months,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[10])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe8m = boost::make_shared<OISRateHelper>(
        2, 8 * Months,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[11])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe9m = boost::make_shared<OISRateHelper>(
        2, 9 * Months,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[12])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe10m = boost::make_shared<OISRateHelper>(
        2, 10 * Months,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[13])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe11m = boost::make_shared<OISRateHelper>(
        2, 11 * Months,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[14])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe1y = boost::make_shared<OISRateHelper>(
        2, 1 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[15])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe15m = boost::make_shared<OISRateHelper>(
        2, 15 * Months,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[16])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe18m = boost::make_shared<OISRateHelper>(
        2, 18 * Months,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[17])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe21m = boost::make_shared<OISRateHelper>(
        2, 21 * Months,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[18])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe2y = boost::make_shared<OISRateHelper>(
        2, 2 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[19])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe3y = boost::make_shared<OISRateHelper>(
        2, 3 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[20])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe4y = boost::make_shared<OISRateHelper>(
        2, 4 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[21])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe5y = boost::make_shared<OISRateHelper>(
        2, 5 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[22])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe6y = boost::make_shared<OISRateHelper>(
        2, 6 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[23])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe7y = boost::make_shared<OISRateHelper>(
        2, 7 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[24])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe8y = boost::make_shared<OISRateHelper>(
        2, 8 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[25])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe9y = boost::make_shared<OISRateHelper>(
        2, 9 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[26])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe10y = boost::make_shared<OISRateHelper>(
        2, 10 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[27])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe11y = boost::make_shared<OISRateHelper>(
        2, 11 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[28])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe12y = boost::make_shared<OISRateHelper>(
        2, 12 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[29])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe15y = boost::make_shared<OISRateHelper>(
        2, 15 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[30])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe20y = boost::make_shared<OISRateHelper>(
        2, 20 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[31])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe25y = boost::make_shared<OISRateHelper>(
        2, 25 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[32])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe30y = boost::make_shared<OISRateHelper>(
        2, 30 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[33])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe40y = boost::make_shared<OISRateHelper>(
        2, 40 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[34])), eoniaBt);
    boost::shared_ptr<RateHelper> rhe50y = boost::make_shared<OISRateHelper>(
        2, 50 * Years,
        Handle<Quote>(boost::make_shared<SimpleQuote>(eoniaquotes[35])), eoniaBt);

    std::vector<boost::shared_ptr<RateHelper> > eoniaratehelpers;
    eoniaratehelpers += rhon, rhe1w, rhe2w, rhe3w, rhe1m, rhe2m, rhe3m, rhe4m,
        rhe5m, rhe6m, rhe7m, rhe8m, rhe9m, rhe10m, rhe11m, rhe1y, rhe15m,
        rhe18m, rhe21m, rhe2y, rhe3y, rhe4y, rhe5y, rhe6y, rhe7y, rhe8y, rhe9y,
        rhe10y, rhe11y, rhe12y, rhe15y, rhe20y, rhe30y, rhe40y, rhe50y;

    Handle<YieldTermStructure> ytsEonia(
        boost::make_shared<PiecewiseYieldCurve<Discount, LogLinear> >(
            0, TARGET(), eoniaratehelpers, Actual365Fixed()));

    ytsEonia->enableExtrapolation();

    Real irquotes[] = {
        0.0013,  0.0013,  0.0013,   0.0016,                   // ON, TN, SN, SW
        // (replace 6m depo by fixing)
        0.0019,  0.0022,  0.0025,   0.0028,  0.0031,  /*0.0035*/ 0.00387, // 1M ... 6M Depo
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

    boost::shared_ptr<IborIndex> euribor6mBt(new Euribor(6 * Months));

    euribor6mBt->addFixing(evalDate, 0.00387); // todays fixing

    // boost::shared_ptr<RateHelper> rhtn = boost::make_shared<DepositRateHelper>(
    //     irquotes[1], 1 * Days, 1, TARGET(), Following, false, Actual360());
    // boost::shared_ptr<RateHelper> rhsn = boost::make_shared<DepositRateHelper>(
    //     irquotes[2], 1 * Days, 2, TARGET(), Following, false, Actual360());
    // boost::shared_ptr<RateHelper> rhsw = boost::make_shared<DepositRateHelper>(
    //     irquotes[3], 1 * Weeks, 2, TARGET(), Following, false, Actual360());
    // boost::shared_ptr<RateHelper> rh1m = boost::make_shared<DepositRateHelper>(
    //     irquotes[4], 1 * Months, 2, TARGET(), ModifiedFollowing, false,
    //     Actual360());
    // boost::shared_ptr<RateHelper> rh2m = boost::make_shared<DepositRateHelper>(
    //     irquotes[5], 2 * Months, 2, TARGET(), ModifiedFollowing, false,
    //     Actual360());
    // boost::shared_ptr<RateHelper> rh3m = boost::make_shared<DepositRateHelper>(
    //     irquotes[6], 3 * Months, 2, TARGET(), ModifiedFollowing, false,
    //     Actual360());
    // boost::shared_ptr<RateHelper> rh4m = boost::make_shared<DepositRateHelper>(
    //     irquotes[7], 4 * Months, 2, TARGET(), ModifiedFollowing, false,
    //     Actual360());
    // boost::shared_ptr<RateHelper> rh5m = boost::make_shared<DepositRateHelper>(
    //     irquotes[8], 5 * Months, 2, TARGET(), ModifiedFollowing, false,
    //     Actual360());
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
        Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
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
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
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
        Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh3y = boost::make_shared<SwapRateHelper>(
        irquotes[28], 3 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh4y = boost::make_shared<SwapRateHelper>(
        irquotes[29], 4 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh5y = boost::make_shared<SwapRateHelper>(
        irquotes[30], 5 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh6y = boost::make_shared<SwapRateHelper>(
        irquotes[31], 6 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh7y = boost::make_shared<SwapRateHelper>(
        irquotes[32], 7 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh8y = boost::make_shared<SwapRateHelper>(
        irquotes[33], 8 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh9y = boost::make_shared<SwapRateHelper>(
        irquotes[34], 9 * Years, TARGET(), Annual, ModifiedFollowing,
        Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh10y =
        boost::make_shared<SwapRateHelper>(irquotes[35], 10 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh11y =
        boost::make_shared<SwapRateHelper>(irquotes[36], 11 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh12y =
        boost::make_shared<SwapRateHelper>(irquotes[37], 12 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh13y =
        boost::make_shared<SwapRateHelper>(irquotes[38], 13 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh14y =
        boost::make_shared<SwapRateHelper>(irquotes[39], 14 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh15y =
        boost::make_shared<SwapRateHelper>(irquotes[40], 15 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh16y =
        boost::make_shared<SwapRateHelper>(irquotes[41], 16 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh17y =
        boost::make_shared<SwapRateHelper>(irquotes[42], 17 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh18y =
        boost::make_shared<SwapRateHelper>(irquotes[43], 18 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh19y =
        boost::make_shared<SwapRateHelper>(irquotes[44], 19 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh20y =
        boost::make_shared<SwapRateHelper>(irquotes[45], 20 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh21y =
        boost::make_shared<SwapRateHelper>(irquotes[46], 21 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh22y =
        boost::make_shared<SwapRateHelper>(irquotes[47], 22 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh23y =
        boost::make_shared<SwapRateHelper>(irquotes[48], 23 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh24y =
        boost::make_shared<SwapRateHelper>(irquotes[49], 24 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh25y =
        boost::make_shared<SwapRateHelper>(irquotes[50], 25 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh26y =
        boost::make_shared<SwapRateHelper>(irquotes[51], 26 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh27y =
        boost::make_shared<SwapRateHelper>(irquotes[52], 27 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh28y =
        boost::make_shared<SwapRateHelper>(irquotes[53], 28 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh29y =
        boost::make_shared<SwapRateHelper>(irquotes[54], 29 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh30y =
        boost::make_shared<SwapRateHelper>(irquotes[55], 30 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh35y =
        boost::make_shared<SwapRateHelper>(irquotes[56], 35 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh40y =
        boost::make_shared<SwapRateHelper>(irquotes[57], 40 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh50y =
        boost::make_shared<SwapRateHelper>(irquotes[58], 50 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);
    boost::shared_ptr<SwapRateHelper> rh60y =
        boost::make_shared<SwapRateHelper>(irquotes[59], 60 * Years, TARGET(),
                                           Annual, ModifiedFollowing,
                                           Thirty360(), euribor6mBt, Handle<Quote>(), 0*Days, ytsEonia);

    std::vector<boost::shared_ptr<RateHelper> > irratehelpers;

    irratehelpers += rh6m, rh1, rh2, rh3, rh4, rh5, rh1y, rh7, rh8, rh9, rh10,
        rh11, rh18m, rh13, rh14, rh15, rh16, rh17, rh2y, rh3y, rh4y, rh5y, rh6y,
        rh7y, rh8y, rh9y, rh10y, rh11y, rh12y, rh13y, rh14y, rh15y, rh16y,
        rh17y, rh18y, rh19y, rh20y, rh21y, rh22y, rh23y, rh24y, rh25y, rh26y,
        rh27y, rh28y, rh29y, rh30y, rh35y, rh40y, rh50y, rh60y;

    Handle<YieldTermStructure> yts6m(
        boost::make_shared<PiecewiseYieldCurve<Discount, LogLinear> >(
            0, TARGET(), irratehelpers, Actual365Fixed()));

    yts6m->enableExtrapolation();

    // set up indices

    boost::shared_ptr<IborIndex> euribor6m = boost::make_shared<Euribor>(6*Months, yts6m);

    boost::shared_ptr<SwapIndex> swap2y = boost::make_shared<EuriborSwapIsdaFixA>(2*Years, yts6m, ytsEonia);
    boost::shared_ptr<SwapIndex> swap5y = boost::make_shared<EuriborSwapIsdaFixA>(5*Years, yts6m, ytsEonia);
    boost::shared_ptr<SwapIndex> swap10y = boost::make_shared<EuriborSwapIsdaFixA>(10*Years, yts6m, ytsEonia);
    boost::shared_ptr<SwapIndex> swap20y = boost::make_shared<EuriborSwapIsdaFixA>(20*Years, yts6m, ytsEonia);
    boost::shared_ptr<SwapIndex> swap30y = boost::make_shared<EuriborSwapIsdaFixA>(30*Years, yts6m, ytsEonia);

    // add todays fixings

    //euribor6m->addFixing(evalDate,0.00387); // alread set above for bootstrapping
    swap2y->addFixing(evalDate,0.00446);
    swap5y->addFixing(evalDate,0.01027);
    swap10y->addFixing(evalDate,0.01904);
    swap20y->addFixing(evalDate,0.02493);
    swap30y->addFixing(evalDate,0.02534);


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

    Real paramGuess[] = {
        0.05, 0.00001, 0.9, 0.0, // 2y und   // 3m
        0.05, 0.00001, 0.9, 0.0,             // 1
        0.05, 0.00001, 0.9, 0.0,             // 5
        0.05, 0.00001, 0.9, 0.0,             // 10 
        0.05, 0.00001, 0.9, 0.0,             // 20
        0.05, 0.00001, 0.9, 0.0,             // 30
        0.05, 0.80, 0.9, 0.0, // 5y
        0.05, 0.80, 0.9, 0.0,
        0.05, 0.80, 0.9, 0.0,
        0.05, 0.80, 0.9, 0.0,
        0.05, 0.80, 0.9, 0.0, 
        0.05, 0.80, 0.9, 0.0,
        0.05, 0.955106, 0.9, 0.0, // 10y
        0.05, 0.955106, 0.9, 0.0,
        0.05, 0.955106, 0.9, 0.0,
        0.05, 0.489027, 0.9, 0.0, 
        0.05, 0.375866, 0.9, 0.0,
        0.05, 0.508918, 0.9, 0.0,
        0.05, 0.80, 0.9, 0.0, // 20y
        0.05, 0.80, 0.9, 0.0,
        0.05, 0.80, 0.9, 0.0, 
        0.05, 0.80, 0.9, 0.0,
        0.05, 0.80, 0.9, 0.0,
        0.05, 0.80, 0.9, 0.0, 
        0.05, 0.80, 0.9, 0.0, // 30y
        0.05, 0.80, 0.9, 0.0, 
        0.05, 0.80, 0.9, 0.0,
        0.05, 0.80, 0.9, 0.0,
        0.05, 0.80, 0.9, 0.0,
        0.05, 0.80, 0.9, 0.0
    };

    // put number of swap tenors, option tenors and strike
    // spreads for smile here

    Size noSwSmile = 5, noOpSmile = 6, noSmileSpreads = 9;

    std::vector<Period> swapTenorsSmile, optionTenorsSmile;
    std::vector<Real> smileSpreads;
    std::vector<std::vector<Handle<Quote> > > swaptionsmilequotes, sabrParams;

    for (int i = 0; i < noSwSmile; i++)
        swapTenorsSmile.push_back(swapTenorsSmileVal[i]);
    for (int i = 0; i < noOpSmile; i++)
        optionTenorsSmile.push_back(optionTenorsSmileVal[i]);
    for (int i = 0; i < noSmileSpreads; i++)
        smileSpreads.push_back(smileSpreadVal[i]);

    for (int i = 0; i < noSwSmile * noOpSmile; i++) {
        std::vector<Handle<Quote> > qSwSmileTmp;
        std::vector<Handle<Quote> > qParam;
        for (int j = 0; j < noSmileSpreads; j++) {
            qSwSmileTmp.push_back(Handle<Quote>(boost::make_shared<SimpleQuote>(
                smileValSpread[i * noSmileSpreads + j] / 100.0)));
        }
        for (int j = 0; j < 4; j++) {
            qParam.push_back(Handle<Quote>(
                boost::make_shared<SimpleQuote>(paramGuess[i * 4 + j])));
        }
        swaptionsmilequotes.push_back(qSwSmileTmp);
        sabrParams.push_back(qParam);
    }

    // we dont have a 3m curve, so we just put 6m here, we don't use these vols anyway
    boost::shared_ptr<SwapIndex> swapIndex(
        new EuriborSwapIsdaFixA(30 * Years, yts6m, ytsEonia));
    boost::shared_ptr<SwapIndex> shortSwapIndex(
        new EuriborSwapIsdaFixA(1 * Years, yts6m, ytsEonia));

    // SABR cube
    std::vector<bool> parameterFixed;
    parameterFixed += false,true,false,false; // beta is fixed
    boost::shared_ptr<EndCriteria> ec = boost::make_shared<EndCriteria>(1000, 100, 1E-8, 1E-8, 1E-8);
    boost::shared_ptr<OptimizationMethod> opt = boost::make_shared<LevenbergMarquardt>(1E-8,1E-8,1E-8);

    Handle<SwaptionVolatilityStructure> swaptionCube(
        boost::shared_ptr<SwaptionVolCube1>(new SwaptionVolCube1(
            swaptionVolAtm, optionTenorsSmile, swapTenorsSmile, smileSpreads,
            swaptionsmilequotes, swapIndex, shortSwapIndex, true, sabrParams,
            parameterFixed, true, ec, 1.00,  opt, // set max error tolerance to high value
            0.0010, false, 50, true)));           // backwardflat beta ?

    // create cms market

    std::vector<Period> swapLengths;
    swapLengths += 5*Years, 10*Years, /*15*Years,*/ 20*Years, 30*Years;
    
    std::vector<boost::shared_ptr<SwapIndex> > swapIndexes;
    swapIndexes += swap2y, swap5y, swap10y, swap20y, swap30y;

    std::vector<Handle<Quote> > margins5y;
    std::vector<Handle<Quote> > margins10y;
    std::vector<Handle<Quote> > margins15y;
    std::vector<Handle<Quote> > margins20y;
    std::vector<Handle<Quote> > margins30y;

    margins5y += 
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00501)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00561)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01077)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01167)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01742)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01842)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01974)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.02174)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01921)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.02171));
    margins10y += 
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00453)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00513)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00885)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00975)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01324)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01424)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01388)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01588)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.0134)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.0159));
    margins15y += 
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00373)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00433)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00682)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00772)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01025)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01125)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01032)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01232)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01021)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01271));
    margins20y += 
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.0032)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.0038)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00574)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00664)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00829)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01029)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00896)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01096)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00883)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01183));
    margins30y += 
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00278)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00338)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00495)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00585)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.0068)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.0088)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00763)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00963)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.00795)),
        Handle<Quote>(boost::make_shared<SimpleQuote>(0.01095));

    std::vector<std::vector<Handle<Quote> > > bidAskSpreads;
    bidAskSpreads += margins5y, margins10y, /*margins15y,*/ margins20y, margins30y;

    // cms pricer, reversion

    Handle<Quote> reversion(boost::make_shared<SimpleQuote>(0.02));

    boost::shared_ptr<CmsCouponPricer> linearTsr =
        boost::make_shared<LinearTsrPricer>(swaptionCube, reversion,
                                            Handle<YieldTermStructure>(), 
                                            LinearTsrPricer::Settings().withVegaRatio(0.01));

    std::vector<boost::shared_ptr<CmsCouponPricer> > pricers(5, linearTsr);

    boost::shared_ptr<CmsMarket> cmsMarket = boost::make_shared<CmsMarket>(swapLengths, swapIndexes, euribor3m,
                                                                           bidAskSpreads, pricers, ytsEonia);

    // cms market calibration
    
    Matrix weights(4,5,1.0); // same weights
    CmsMarketCalibration cmsCalibration(swaptionCube, cmsMarket, weights, CmsMarketCalibration::OnSpread);
    
    // Array guess(6,0.8); // beta guess (no termstructure)
    // guess[5] = 0.03;
    Matrix guess(4,5,0.8); // beta guess (termstructure)
    // guess[0][0] = 0.95;
    // guess[1][0] = 0.50;
    // guess[2][0] = 0.40;
    // guess[3][0] = 0.45;
    // guess[4][0] = 0.01;

    boost::shared_ptr<EndCriteria> ec2 = boost::make_shared<EndCriteria>(500,50,1E-8,1E-8,1E-8);
    boost::shared_ptr<OptimizationMethod> opt2 = boost::make_shared<LevenbergMarquardt>(1E-8,1E-8,1E-8);
    //boost::shared_ptr<OptimizationMethod> opt2 = boost::make_shared<Simplex>(0.60);

    // Array result = cmsCalibration.compute(ec2, opt2, guess, true);
    Matrix result; //= cmsCalibration.compute(ec2, opt2, guess, true);

    // output results

    std::cout << "Calibration results:" << std::endl;
    // no ts
    // for(Size i=0;i<result.size();i++) std::cout << result[i] << ";";
    // std::cout << std::endl;
    // ts
    for(Size i=0;i<result.rows();++i) {
        for(Size j=0;j<result.columns();++j) {
            std::cout << result[i][j] << ";";
        }
        std::cout << std::endl;
    }

    std::cout << "index;maturity;bid;ask;mid;model;err;errBidAsk;npvMkt;" <<
                 "npvMdl;npvErr;npvFwdMkt;npvFwdMdl;npvFwdErr" << std::endl;
    Matrix res = cmsMarket->browse();
    for (Size i = 0; i < res.rows(); i++) {
        for (Size j = 0; j < res.columns(); j++) {
            std::cout << res[i][j] << ";";
        }
        std::cout << std::endl;
    }

    std::cout << "dense cube parameters" << std::endl;

    Matrix dense = boost::dynamic_pointer_cast<SwaptionVolCube1>(*swaptionCube)->denseSabrParameters();
    std::cout << "swap;option;alpha;beta;nu;rho;forward;error;maxError;ec" << std::endl;
    for(Size i=0;i<dense.rows();i++) {
        for(Size j=0;j<dense.columns();j++) {
            std::cout << dense[i][j] << ";";
        }
        std::cout << std::endl;
    }

    std::cout << "recalculate model margins for input cms swaps" << std::endl;

    for(Size j=0;j<swapIndexes.size();j++) {
        for(Size i=0;i<swapLengths.size();i++) {
            boost::shared_ptr<Swap> swap = MakeCms(swapLengths[i],swapIndexes[j],euribor6m,0.0,Period())
                .withCmsCouponPricer(pricers[j])
                .withDiscountingTermStructure(ytsEonia);
            Real floatLegNpv = CashFlows::npv(swap->leg(1),**ytsEonia,false);
            Real floatLegBps = CashFlows::bps(swap->leg(1),**ytsEonia,false);
            Real cmsLegNpv = CashFlows::npv(swap->leg(0),**ytsEonia,false);
            Real fairMargin = - (floatLegNpv - cmsLegNpv) / floatLegBps;
            std::cout << fairMargin << std::endl;
        }
    }

    std::cout << "output some sample smiles relevant for cms pricing" << std::endl;
    Real spread = -0.10;
    std::cout << "date;atm;";
    while (spread <= 1.0) {
        std::cout << spread << ";";
        spread += 0.01;
    }
    std::cout << std::endl;

    boost::shared_ptr<Swap> swap = MakeCms(swapLengths[0],swapIndexes[0],euribor6m,0.0,Period())
        .withCmsCouponPricer(pricers[0])
        .withDiscountingTermStructure(ytsEonia);

    std::cout << "swapLength = " << swapLengths[0] << " index = " << swapIndexes[0]->tenor() << std::endl;

    Leg cmsLeg = swap->leg(0);
    for(Size i=0;i<cmsLeg.size();i++) {
        boost::shared_ptr<CmsCoupon> c = boost::dynamic_pointer_cast<CmsCoupon>(cmsLeg[i]);
        Date fixingDate = c->fixingDate();
        boost::shared_ptr<SmileSection> sec =
            swaptionCube->smileSection(fixingDate, c->swapIndex()->tenor());
        Real atm = boost::dynamic_pointer_cast<SwaptionVolatilityCube>(
                       *swaptionCube)->atmStrike(fixingDate, 10 * Years);
        std::cout << fixingDate << ";" << atm << ";";
        Real spread = -0.10;
        while (spread <= 1.0) {
            if (atm + spread > 0.0)
                std::cout << sec->volatility(atm + spread) << ";";
            else
                std::cout << "0.0;";
            spread += 0.01;
        }
        std::cout << std::endl;
    }
}


