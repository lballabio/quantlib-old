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
        std::vector<Real> volsteptimes;
        Array volsteptimes_a(0);
        std::vector<Real> eurVols(1, 0.0010);
        std::vector<Real> usdVols(1, 0.0025);
        std::vector<Real> fxSigmas(1, 0.20);
        Array fxSigmas_a(fxSigmas.begin(), fxSigmas.end());

        boost::shared_ptr<Lgm1> eurLgm =
            boost::make_shared<Lgm1>(eurYts, volstepdates, eurVols, 0.0020);
        boost::shared_ptr<Lgm1> usdLgm =
            boost::make_shared<Lgm1>(usdYts, volstepdates, usdVols, 0.0080);

        std::vector<boost::shared_ptr<Lgm1> > singleModels;
        singleModels.push_back(eurLgm);
        singleModels.push_back(usdLgm);

        std::vector<Handle<YieldTermStructure> > curves;
        curves.push_back(eurYts);
        curves.push_back(usdYts);

        // build cc parametrization from scratch

        // lgm parametrizations

        std::vector<boost::shared_ptr<detail::LgmParametrization<
            detail::LgmPiecewiseAlphaConstantKappa> > > lgmParametrizations;

        boost::shared_ptr<
            detail::LgmParametrization<detail::LgmPiecewiseAlphaConstantKappa> >
            eurParam = eurLgm->parametrization();
        boost::shared_ptr<
            detail::LgmParametrization<detail::LgmPiecewiseAlphaConstantKappa> >
            usdParam = usdLgm->parametrization();

        lgmParametrizations.push_back(eurParam);
        lgmParametrizations.push_back(usdParam);

        // fx parametrizations

        std::vector<boost::shared_ptr<detail::LgmFxParametrization<
            detail::LgmFxPiecewiseSigma> > > fxParametrizations;

        boost::shared_ptr<detail::LgmFxParametrization<
            detail::LgmFxPiecewiseSigma> > fxParam =
            boost::make_shared<detail::LgmFxPiecewiseSigma>(volsteptimes_a,
                                                            fxSigmas_a);

        fxParametrizations.push_back(fxParam);

        // the fx vols, correlations and the cclgmm parametrization / process /
        // model

        std::vector<Handle<Quote> > fxSpots;
        fxSpots.push_back(Handle<Quote>(boost::make_shared<SimpleQuote>(
            std::log(0.9090)))); // EUR-USD ~ 1.10

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

        boost::shared_ptr<detail::CcLgmPiecewise> ccParam =
            boost::make_shared<detail::CcLgmPiecewise>(fxParametrizations,
                                                       lgmParametrizations, c);

        boost::shared_ptr<
            CcLgmProcess<detail::CcLgmPiecewise, detail::LgmFxPiecewiseSigma,
                         detail::LgmPiecewiseAlphaConstantKappa> > process =
            boost::make_shared<CcLgmProcess<
                detail::CcLgmPiecewise, detail::LgmFxPiecewiseSigma,
                detail::LgmPiecewiseAlphaConstantKappa> >(ccParam, fxSpots,
                                                          curves);

        // generate paths

        Size n = 1;
        Size steps = 10;
        TimeGrid grid(5.0, steps);

        // boost::shared_ptr<StochasticProcess> process =
        // multiGsr->stateProcess();

        PseudoRandom::rsg_type sg =
            PseudoRandom::make_sequence_generator(steps * 3, 14);
        MultiPathGenerator<PseudoRandom::rsg_type> pg(process, grid, sg, false);

        std::vector<Sample<MultiPath> > paths;
        for (Size j = 0; j < n; ++j) {
            paths.push_back(pg.next());
        }

        for (Size i = 0; i < steps; ++i) {
            std::cout << grid[i] << " ";
            for (Size j = 0; j < n; ++j) {
                std::cout << std::exp(paths[j].value[0][i]) << " "
                          << paths[j].value[1][i] << " " << paths[j].value[2][i]
                          << " ";
            }
            std::cout << "\n";
        }

        return 0;

    } catch (QuantLib::Error e) {
        std::clog << "ql exception : " << e.what();
    } catch (std::exception e) {
        std::clog << "std exception: " << e.what();
    }
}
