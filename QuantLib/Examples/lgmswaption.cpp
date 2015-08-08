#include <ql/quantlib.hpp>

using namespace QuantLib;

int main() {

    try {

        Real rateLevel = 0.02;
        Date evalDate(12, January, 2015);

        Settings::instance().evaluationDate() = evalDate;

        std::clog << "reference date " << evalDate << std::endl;

        Handle<YieldTermStructure> yts(boost::make_shared<FlatForward>(
            evalDate, rateLevel, Actual365Fixed()));

        boost::shared_ptr<IborIndex> euribor6m =
            boost::make_shared<Euribor>(6 * Months, yts);

        Real strike = 0.02;

        Date effectiveDate = TARGET().advance(evalDate, 2 * Days);
        Date startDate = TARGET().advance(effectiveDate, 1 * Years);
        Date maturityDate = TARGET().advance(startDate, 9 * Years);

        Schedule fixedSchedule(startDate, maturityDate, 1 * Years, TARGET(),
                               ModifiedFollowing, ModifiedFollowing,
                               DateGeneration::Forward, false);
        Schedule floatingSchedule(startDate, maturityDate, 6 * Months, TARGET(),
                                  ModifiedFollowing, ModifiedFollowing,
                                  DateGeneration::Forward, false);

        boost::shared_ptr<VanillaSwap> underlying =
            boost::make_shared<VanillaSwap>(VanillaSwap(
                VanillaSwap::Payer, 1.0, fixedSchedule, strike, Thirty360(),
                floatingSchedule, euribor6m, 0.0, Actual360()));

        boost::shared_ptr<PricingEngine> discountingEngine =
            boost::make_shared<DiscountingSwapEngine>(yts);
        underlying->setPricingEngine(discountingEngine);

        std::clog << "underlying price = " << underlying->NPV() << std::endl;
        std::clog << "fair rate = " << underlying->fairRate() << std::endl;

        std::vector<Date> exerciseDates;
        std::clog << "exercise dates:" << std::endl;
        for (Size i = 0; i < 9; ++i) {
            exerciseDates.push_back(
                TARGET().advance(fixedSchedule[i], -2 * Days));
            std::clog << exerciseDates.back() << "\n";
        }

        boost::shared_ptr<Exercise> exercise =
            boost::make_shared<BermudanExercise>(exerciseDates, false);

        boost::shared_ptr<Swaption> swaption =
            boost::make_shared<Swaption>(underlying, exercise);

        std::vector<Date> stepDates(exerciseDates.begin(),
                                    exerciseDates.end() - 1);

        std::vector<Real> sigmas(stepDates.size() + 1, 0.0050);
        Real reversion = 0.0;

        boost::shared_ptr<Gsr> gsr =
            boost::make_shared<Gsr>(yts, stepDates, sigmas, reversion, 50.0);

        boost::shared_ptr<Lgm1> lgm =
            boost::make_shared<Lgm1>(yts, stepDates, sigmas, reversion);

        boost::shared_ptr<PricingEngine> swaptionEngineGsr =
            boost::make_shared<Gaussian1dSwaptionEngine>(
                gsr, 64, 7.0, true, false);

        boost::shared_ptr<PricingEngine> swaptionEngineLgm =
            boost::make_shared<Gaussian1dSwaptionEngine>(
                lgm, 64, 7.0, true, false);

        swaption->setPricingEngine(swaptionEngineGsr);
        Real npvGsr = swaption->NPV();
        swaption->setPricingEngine(swaptionEngineLgm);
        Real npvLgm = swaption->NPV();

        std::clog.precision(16);
        std::clog << "npv (Gsr) = " << npvGsr << std::endl;
        std::clog << "npv (Lgm) = " << npvLgm << std::endl;

        return 0;

    } catch (QuantLib::Error e) {
        std::clog << e.what() << std::endl;
    } catch (std::exception e) {
        std::clog << e.what() << std::endl;
    }

} // main
