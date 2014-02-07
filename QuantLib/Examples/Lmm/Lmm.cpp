#include <ql/quantlib.hpp>
#include <iostream>

using namespace QuantLib;

const double LTCORR = 0.2;    // long term correlations
const double CORRDECAY = 0.1; // correlation decay

const Size FACTORS = 10; // number of factors
const long SEED = 42;    // seed for rng

const Size CALIBRATIONPATHS = 10000; // simulation paths in calibration
const Size PRICINGPATHS = 1000;      // simulation paths in pricing

int main(int, char **) {

    bool calibrate = true; // calibrate the model ?
    bool price = false;    // price OTC swap ?

    std::cout << "Libor Market Model Demo --- CMS Swaption Pricing"
              << std::endl;

    // set evalulation date

    Date evalDate = Date(4, February, 2014);
    Date settlDate = TARGET().advance(evalDate, 2 * Days);
    Settings::instance().evaluationDate() = evalDate;

    // set up the interest rate curve (6m curve with deposit on short end to fix
    // a unique curve for everything)

    Real irquotevalues[] = {
        0.0455, 0.0472, 0.0492, 0.0519, 0.0524, // deposit quotes
        0.0516, 0.0485, 0.0481, 0.0480, 0.0480, 0.0480, 0.0481, 0.0482,
        0.0483, 0.0486, 0.0491, 0.0496, 0.0496, 0.0493, 0.0489
    }; // swap quotes

    Period tenors[] = { 1 * Months, 2 * Months, 3 * Months, 6 * Months,
                        9 * Months, // deposits
                        1 * Years,  2 * Years,  3 * Years,  4 * Years,
                        5 * Years,  6 * Years,  7 * Years,  8 * Years,
                        9 * Years,  10 * Years, 12 * Years, 15 * Years,
                        20 * Years, 25 * Years, 30 * Years }; // swaps

    Size noDep = 5, noSwap = 15; // put number of deposits and swaps here

    std::vector<boost::shared_ptr<Quote> > irquotes;
    for (int i = 0; i < noDep + noSwap; i++) {
        irquotes.push_back(
            boost::shared_ptr<Quote>(new SimpleQuote(irquotevalues[i])));
    }

    // index without yieldcurve assigned for swaprate helpers
    boost::shared_ptr<IborIndex> euribor6mBootstrap(new Euribor(6 * Months));

    std::vector<boost::shared_ptr<RateHelper> > irratehelpers;
    // check: fixing days = 2 for all tenors ?
    for (int i = 0; i < noDep; i++)
        irratehelpers.push_back(boost::shared_ptr<RateHelper>(
            new DepositRateHelper(Handle<Quote>(irquotes[i]), tenors[i], 2,
                                  TARGET(), ModifiedFollowing, false,
                                  Actual360())));
    for (int i = noDep; i < noDep + noSwap; i++)
        irratehelpers.push_back(boost::shared_ptr<RateHelper>(
            new SwapRateHelper(Handle<Quote>(irquotes[i]), tenors[i], TARGET(),
                               Annual, ModifiedFollowing, Thirty360(),
                               euribor6mBootstrap)));

    // discount is 1.0 on TODAY (evalDate)
    boost::shared_ptr<YieldTermStructure> yts(
        new PiecewiseYieldCurve<ZeroYield, Linear>(0, TARGET(), irratehelpers,
                                                   Actual365Fixed()));
    yts->enableExtrapolation();

    // set up the swaption vol cube

    // atm

    Period swapTenorsVal[] = { 20 * Years, 25 * Years, 30 * Years };
    Period optionTenorsVal[] = { 3 * Months, 6 * Months, 1 * Years, 2 * Years,
                                 3 * Years,  4 * Years,  5 * Years, 7 * Years };
    Size noSwAtm = 3,
         noOpAtm = 8; // put number of atm swap tenors and option tenors here

    Real swaptionatmvalues[] = {
        0.1795, 0.1780, 0.1755, // option tenor 1 3m into 20y, 25y, 30y
        0.1635, 0.1630, 0.1615, // option tenor 2 6m
        0.1470, 0.1460, 0.1460, //                1y
        0.1350, 0.1350, 0.1350, //                2y
        0.1295, 0.1300, 0.1300, //                3y
        0.1270, 0.1270, 0.1270, //              4y
        0.1235, 0.1240, 0.1240, //              5y
        0.1165, 0.1160, 0.1145
    }; //            7y

    std::vector<Period> optionTenors, swapTenors;
    std::vector<std::vector<Handle<Quote> > > swatmquotes;
    for (int i = 0; i < noOpAtm; i++) {
        optionTenors.push_back(optionTenorsVal[i]);
        std::vector<Handle<Quote> > swatmtemp;
        for (int j = 0; j < noSwAtm; j++) {
            if (i == 0)
                swapTenors.push_back(swapTenorsVal[j]);
            swatmtemp.push_back(Handle<Quote>(boost::shared_ptr<Quote>(
                new SimpleQuote(swaptionatmvalues[i * noSwAtm + j]))));
        }
        swatmquotes.push_back(swatmtemp);
    }

    boost::shared_ptr<SwaptionVolatilityStructure> swaptionVolAtm(
        new SwaptionVolatilityMatrix(TARGET(), ModifiedFollowing, optionTenors,
                                     swapTenors, swatmquotes,
                                     Actual365Fixed()));

    // smile
    Period swapTenorsSmileVal[] = { 20 * Years, 25 * Years, 30 * Years };
    Period optionTenorsSmileVal[] = { 3 * Months, 6 * Months, 1 * Years,
                                      2 * Years,  3 * Years,  4 * Years,
                                      5 * Years,  7 * Years };
    Real smileSpreadVal[] = { -0.02, -0.01, 0.00, 0.01, 0.02 };

    Real smileValSpread[] = {
        0.2725, 0.2161, 0.00,
        0.1706, 0.1741, // opt1swp1 3m20y // this is NOT the spread,
                        // but absolute vols, except ATM which is zero !
        0.2711, 0.2147, 0.00,
        0.1691, 0.1727, // opt1swp2 3m25y
        0.2684, 0.2121, 0.00,
        0.1667, 0.1704, // opt1swp3 3m30y
        0.2484, 0.1970, 0.00,
        0.1554, 0.1586, // opt2     6m
        0.2485, 0.1967, 0.00,
        0.1549, 0.1582, //
        0.2472, 0.1953, 0.00,
        0.1533, 0.1568, //
        0.2329, 0.1840, 0.00,
        0.1366, 0.1335, // opt3     1y
        0.2322, 0.1831, 0.00,
        0.1356, 0.1326, //
        0.2333, 0.1834, 0.00,
        0.1355, 0.1327, //
        0.1684, 0.1472, 0.00,
        0.1295, 0.1284, // opt4     2y
        0.1689, 0.1474, 0.00,
        0.1295, 0.1284, //
        0.1695, 0.1475, 0.00,
        0.1294, 0.1284, //
        0.1688, 0.1442, 0.00,
        0.1256, 0.1252, // opt5     3y
        0.1702, 0.1450, 0.00,
        0.1261, 0.1257, //
        0.1710, 0.1452, 0.00,
        0.1261, 0.1257, //
        0.1681, 0.1409, 0.00,
        0.1247, 0.1250, // opt6     4y
        0.1689, 0.1411, 0.00,
        0.1246, 0.1250, //
        0.1697, 0.1414, 0.00,
        0.1246, 0.1250, //
        0.1640, 0.1373, 0.00,
        0.1210, 0.1212, // opt7     5y
        0.1656, 0.1381, 0.00,
        0.1214, 0.1217, //
        0.1663, 0.1384, 0.00,
        0.1214, 0.1217, //
        0.1532, 0.1311, 0.00,
        0.1124, 0.1124, // opt8     7y
        0.1534, 0.1308, 0.00,
        0.1119, 0.1120, //
        0.1519, 0.1293, 0.00,
        0.1104, 0.1105
    };

    Size noSwSmile = 3, noOpSmile = 8,
         noSmileSpreads =
             5; // put number of swap tenors, option tenors and strike
                // spreads for smile here

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
            qSwSmileTmp.push_back(Handle<Quote>(boost::shared_ptr<Quote>(
                new SimpleQuote(smileValSpread[i * noSmileSpreads + j]))));
        }
        swaptionsmilequotes.push_back(qSwSmileTmp);
    }

    boost::shared_ptr<SwapIndex> swapIndex(
        new EuriborSwapIsdaFixA(30 * Years, Handle<YieldTermStructure>(yts)));
    boost::shared_ptr<SwapIndex> shortSwapIndex(
        new EuriborSwapIsdaFixA(1 * Years, Handle<YieldTermStructure>(yts)));

    // we use the linearly interpolated cube, which is enough in this context
    boost::shared_ptr<SwaptionVolatilityStructure> swaptionCube(
        new SwaptionVolCube2(
            Handle<SwaptionVolatilityStructure>(swaptionVolAtm),
            optionTenorsSmile, swapTenorsSmile, smileSpreads,
            swaptionsmilequotes, swapIndex, shortSwapIndex,
            false)); // linear interpolated cube

    // times, accruals, forwards setup
    Date termDate = Date(29, July, 2046);

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

        CalibrationError(
            const boost::shared_ptr<YieldTermStructure> &yts,
            const boost::shared_ptr<SwaptionVolatilityStructure> &vcube,
            const std::vector<Real> &rateTimes,
            const std::vector<Real> &todaysForwards)
            : yts_(yts), vcube_(vcube), rateTimes_(rateTimes),
              todaysForwards_(todaysForwards) {

            // swap fixed leg is covering step periods of forwards rates
            const Size step = 2;

            // calibration basket definition
            const Size swpFixIdx[] = { 39,39,39,39,
                                       40,40,40,40,
                                       41,41,41,41,
                                       42,42,42,42,
                                       43,43,43,43,
                                       44,44,44,44,
                                       45,45,45,45,
                                       46,46,46,46,
                                       47,47,47,47,
                                       48,48,48,48,
                                       49,49,49,49,
                                       50,50,50,50,
                                       51,51,51,51,
                                       52,52,52,52,
                                       53,53,53,53,
                                       54,54,54,54,
                                       55,55,55,55,
                                       56,56,56,56,
                                       57,57,57,57,
                                       58,58,58,58,
                                       59,59,59,59,
                                       60,60,60,60,
                                       61,61,61,61,
                                       62,62,62,62,
                                       63,63,63,63,
                                       64,64,64,64 };

            const Size swpEndIdx[] = { 52, 52, 52, 52, 52, 52, 52, 52, 52,
                                       52, 52, 52, 52, 52, 52, 52, 52, 52,
                                       52, 52, 52, 52, 52, 52, 52, 52, 52,
                                       52, 52, 52, 52, 52, 52, 52, 52 };
            const Real atmOffset[] = { -0.02, -0.01, 0.0,   0.01,  0.02,  -0.02,
                                       -0.01, 0.0,   0.01,  0.02,  -0.02, -0.01,
                                       0.0,   0.01,  0.02,  -0.02, -0.01, 0.0,
                                       0.01,  0.02,  -0.02, -0.01, 0.0,   0.01,
                                       0.02,  -0.02, -0.01, 0.0,   0.01,  0.02,
                                       -0.02, -0.01, 0.0,   0.01,  0.02 };

            Size noCalInstr = 35;

            LMMCurveState curve0(rateTimes);
            curve0.setOnForwardRates(todaysForwards);
            Real d0 = yts->discount(rateTimes[0]);

            for (Size i = 0; i < noCalInstr; i++) {
                strikes_.push_back(
                    curve0.swapRate(swpFixIdx[i], swpEndIdx[i], step) +
                    atmOffset[i]);
                forwards_.push_back(
                    curve0.swapRate(swpFixIdx[i], swpEndIdx[i], step));
                annuities_.push_back(d0 * curve0.swapAnnuity(0, swpFixIdx[i],
                                                             swpEndIdx[i],
                                                             step));
                fixingTimes_.push_back(rateTimes[swpFixIdx[i]]);
                boost::shared_ptr<StrikedTypePayoff> payoff(
                    new PlainVanillaPayoff(Option::Call, strikes_[i]));
                MultiStepSwaption calInstr(rateTimes, swpFixIdx[i],
                                           swpEndIdx[i], payoff, step);
                marketVols_.push_back(vcube_->volatility(
                    fixingTimes_[i], swpEndIdx[i] / 2.0, strikes_[i]));
                marketPrices_.push_back(blackFormula(
                    Option::Call, strikes_[i], forwards_[i],
                    sqrt(fixingTimes_[i]) * marketVols_[i], annuities_[i]));
                weights_.push_back(1.0);
                calibrationInstruments_.add(calInstr);
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
        }

        Disposable<Array>
        operator()(const std::vector<Real> &displacements,
                   const std::vector<Real> &volatilities, const Real a,
                   const Real b, const Real c, const Real d, const Real volvol,
                   const Real meanRev,
                   const Size paths = CALIBRATIONPATHS) const {
            // put default paths here (used in calibration)

            boost::shared_ptr<MarketModel> model(new AbcdVol(
                a, b, c, d, volatilities, corr_, evolution_, FACTORS,
                todaysForwards_, displacements)); // factors = 10
            long seed = SEED;
            MTBrownianGeneratorFactory generatorFactory(
                seed); // what rng do we use ? MT or SOBOL ?
            // SobolBrownianGeneratorFactory
            // generatorFactory(SobolBrownianGenerator::Diagonal, seed);

            boost::shared_ptr<MarketModelEvolver> evolver;

            if (close(volvol, 0.0)) { // the criterion whether to use a shifted
                                      // bgm or a stoch vol bgm is the volvol
                // SBGM
                evolver = boost::shared_ptr<MarketModelEvolver>(
                    new LogNormalFwdRatePc(model, generatorFactory, numeraires_,
                                           0));
                // what does initial step mean?
                // boost::shared_ptr<MarketModelEvolver> evolver(new
                // LogNormalFwdRateEuler(model, generatorFactory,
                // numeraires_,0));
                // what does initial step mean?
            } else {
                // FLSV
                boost::shared_ptr<MarketModelVolProcess> volProcess(
                    new SquareRootAndersen(
                        1.0, meanRev, volvol, 1.0, evolution_.evolutionTimes(),
                        8, 0.5, 0.5, 1.5)); // fancy parameters here ?
                evolver = boost::shared_ptr<MarketModelEvolver>(
                    new SVDDFwdRatePc(model, generatorFactory, volProcess, 2, 2,
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
            std::cout << "#\tfixingT\tstrike \tpricemk\tpricemd\tvolmk  "
                         "\tvolmd  \tweight // " << std::endl;
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
                    weights_[i] * (modelp - marketp); // measure error in terms of price difference 
                std::cout
                    << i << "\t" << fixingTimes_[i] << "\t" << strikes_[i]
                    << "\t" << marketp << "\t" << modelp << "\t" << marketv
                    << "\t" << modelv << "\t" << weights_[i] << std::endl;
            }

            return result;
        }

        const boost::shared_ptr<YieldTermStructure> &yts_;
        const boost::shared_ptr<SwaptionVolatilityStructure> &vcube_;
        const std::vector<Real> &rateTimes_;
        const std::vector<Real> &todaysForwards_;
        MultiProductComposite calibrationInstruments_;
        EvolutionDescription evolution_;
        std::vector<Real> marketPrices_, marketVols_, strikes_, forwards_,
            annuities_, fixingTimes_, weights_;
        std::vector<Size> numeraires_;
        boost::shared_ptr<PiecewiseConstantCorrelation> corr_;
    };

    return 0;

}
