#include <ql/quantlib.hpp>

#include <boost/make_shared.hpp>

// example / tests for multicurrency lgm model

using namespace QuantLib;

void nodelete() {}

int main() {

    try {

        Date referenceDate(30, July, 2015);

        Settings::instance().evaluationDate() = referenceDate;

        // the single currency models
        // they can be calibrated in the usual way

        Handle<YieldTermStructure> eurYts(boost::make_shared<FlatForward>(
            referenceDate, 0.01, Actual365Fixed()));

        Handle<YieldTermStructure> usdYts(boost::make_shared<FlatForward>(
            referenceDate, 0.02, Actual365Fixed()));

        std::vector<Date> volstepdates;
        std::vector<Real> eurVols(1, 0.0010);
        std::vector<Real> usdVols(1, 0.0025);

        boost::shared_ptr<Lgm1> eurLgm =
            boost::make_shared<Lgm1>(eurYts, volstepdates, eurVols, 0.0020);
        boost::shared_ptr<Lgm1> usdLgm =
            boost::make_shared<Lgm1>(usdYts, volstepdates, usdVols, 0.0080);

        std::vector<boost::shared_ptr<Lgm1> > singleModels;
        singleModels.push_back(eurLgm);
        singleModels.push_back(usdLgm);

        boost::shared_ptr<
            LgmParametrization<detail::LgmPiecewiseAlphaConstantKappa> >
            eurParam = boost::make_shared<
                LgmParametrization<detail::LgmPiecewiseAlphaConstantKappa> >(
                eurLgm->parametrization());

        // the fx vols, correlations and the multi currency model

        std::vector<Real> fxSpots;
        fxSpots.push_back(0.9090); // EUR-USD ~ 1.10

        std::vector<Date> fxVolStepDates;
        std::vector<std::vector<Real> > fxVols(1, std::vector<Real>(1, 0.10));

        Matrix c(3, 3);
        //  FX             EUR         USD
        c[0][0] = 1.0;
        c[0][1] = 0.0;
        c[0][2] = 0.0; // FX
        c[1][0] = 0.0;
        c[1][1] = 1.0;
        c[1][2] = 0.95; // EUR
        c[2][0] = 0.0;
        c[2][1] = 0.95;
        c[2][2] = 1.0; // USD

        // generate paths

        // Size n = 100;
        // Size steps = 100;
        // TimeGrid grid(5.0, steps);

        // boost::shared_ptr<StochasticProcess> process =
        // multiGsr->stateProcess();

        // PseudoRandom::rsg_type sg =
        //     PseudoRandom::make_sequence_generator(steps * 3, 14);
        // MultiPathGenerator<PseudoRandom::rsg_type> pg(process, grid, sg,
        // false);

        // std::vector<Sample<MultiPath> > paths;
        // for (Size j = 0; j < n; ++j) {
        //     paths.push_back(pg.next());
        // }

        // for (Size i = 0; i < steps; ++i) {
        //     std::cout << grid[i] << " ";
        //     for (Size j = 0; j < n; ++j) {
        //         std::cout << paths[j].value[0][i] << " " <<
        //         paths[j].value[1][i]
        //                   << " " << paths[j].value[2][i] << " ";
        //     }
        //     std::cout << "\n";
        // }

        return 0;

    } catch (QuantLib::Error e) {
        std::clog << "ql exception : " << e.what();
    } catch (std::exception e) {
        std::clog << "std exception: " << e.what();
    }
}
