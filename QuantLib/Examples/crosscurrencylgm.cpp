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
            referenceDate, 0.02, Actual365Fixed()));

        Handle<YieldTermStructure> usdYts(boost::make_shared<FlatForward>(
            referenceDate, 0.05, Actual365Fixed()));

        std::vector<Date> volstepdates;
        std::vector<Real> volsteptimes;
        Array volsteptimes_a(0);
        std::vector<Real> eurVols(1, atof(getenv("EURVOL")));
        std::vector<Real> usdVols(1, atof(getenv("USDVOL")));
        std::vector<Real> fxSigmas(1, atof(getenv("FXVOL")));
        Array fxSigmas_a(fxSigmas.begin(), fxSigmas.end());

        boost::shared_ptr<Lgm1> eurLgm =
            boost::make_shared<Lgm1>(eurYts, volstepdates, eurVols, 0.0000);
        boost::shared_ptr<Lgm1> usdLgm =
            boost::make_shared<Lgm1>(usdYts, volstepdates, usdVols, 0.0000);

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
        // c[0][0] = 1.0; c[0][1] = -0.8; c[0][2] = 0.8; // FX
        // c[1][0] = -0.8; c[1][1] = 1.0; c[1][2] = -0.5; // EUR
        // c[2][0] = 0.8; c[2][1] = -0.5; c[2][2] = 1.0; // USD
        //  FX             EUR         USD
        // c[0][0] = 1.0; c[0][1] = 0.8; c[0][2] = 0.8; // FX
        // c[1][0] = 0.8; c[1][1] = 1.0; c[1][2] = 0.5; // EUR
        // c[2][0] = 0.8; c[2][1] = 0.5; c[2][2] = 1.0; // USD
        //  FX             EUR         USD
        c[0][0] = 1.0; c[0][1] = 0.0; c[0][2] = 0.0; // FX
        c[1][0] = 0.0; c[1][1] = 1.0; c[1][2] = 0.0; // EUR
        c[2][0] = 0.0; c[2][1] = 0.0; c[2][2] = 1.0; // USD

        boost::shared_ptr<detail::CcLgmPiecewise> ccParam =
            boost::make_shared<detail::CcLgmPiecewise>(fxParametrizations,
                                                       lgmParametrizations, c);

        ccParam->update();

        // test parametrization

        std::clog.precision(12);
        // std::clog << "H0(0.0) = " << ccParam->H_i(0,0.0) << std::endl;
        // std::clog << "H0(1.0) = " << ccParam->H_i(0,1.0) << std::endl;
        // std::clog << "H0(2.0) = " << ccParam->H_i(0,2.0) << std::endl;
        // std::clog << "H1(0.0) = " << ccParam->H_i(1,0.0) << std::endl;
        // std::clog << "H1(1.0) = " << ccParam->H_i(1,1.0) << std::endl;
        // std::clog << "H1(2.0) = " << ccParam->H_i(1,2.0) << std::endl;
        // std::clog << "zeta0(0.0) = " << ccParam->zeta_i(0,0.0) << std::endl;
        // std::clog << "zeta0(1.0) = " << ccParam->zeta_i(0,1.0) << std::endl;
        // std::clog << "zeta0(2.0) = " << ccParam->zeta_i(0,2.0) << std::endl;
        // std::clog << "zeta0(3.0) = " << ccParam->zeta_i(0,3.0) << std::endl;
        // std::clog << "zeta1(0.0) = " << ccParam->zeta_i(1,0.0) << std::endl;
        // std::clog << "zeta1(1.0) = " << ccParam->zeta_i(1,1.0) << std::endl;
        // std::clog << "zeta1(2.0) = " << ccParam->zeta_i(1,2.0) << std::endl;
        // std::clog << "zeta1(3.0) = " << ccParam->zeta_i(1,3.0) << std::endl;
        // std::clog << "alphaialphaj(0.0) = " <<
        // ccParam->alpha_i_alpha_j(0,0,0.0) << std::endl;
        // std::clog << "alphaialphaj(1.0) = " <<
        // ccParam->alpha_i_alpha_j(0,0,1.0) << std::endl;
        // std::clog << "alphaialphaj(2.0) = " <<
        // ccParam->alpha_i_alpha_j(0,0,2.0) << std::endl;
        // std::clog << "alphaialphaj(0.0) = " <<
        // ccParam->alpha_i_alpha_j(1,1,0.0) << std::endl;
        // std::clog << "alphaialphaj(1.0) = " <<
        // ccParam->alpha_i_alpha_j(1,1,1.0) << std::endl;
        // std::clog << "alphaialphaj(2.0) = " <<
        // ccParam->alpha_i_alpha_j(1,1,2.0) << std::endl;
        // std::clog << "alphaialphaj(0.0) = " <<
        // ccParam->alpha_i_alpha_j(0,1,0.0) << std::endl;
        // std::clog << "alphaialphaj(1.0) = " <<
        // ccParam->alpha_i_alpha_j(0,1,1.0) << std::endl;
        // std::clog << "alphaialphaj(2.0) = " <<
        // ccParam->alpha_i_alpha_j(0,1,2.0) << std::endl;
        // std::clog << "alphaialphaj(0.0) = " <<
        // ccParam->alpha_i_alpha_j(1,0,0.0) << std::endl;
        // std::clog << "alphaialphaj(1.0) = " <<
        // ccParam->alpha_i_alpha_j(1,0,1.0) << std::endl;
        // std::clog << "alphaialphaj(2.0) = " <<
        // ccParam->alpha_i_alpha_j(1,0,2.0) << std::endl;
        // std::clog << "sigmaisigmaj(0.0) = " <<
        // ccParam->sigma_i_sigma_j(0,0,0.0) << std::endl;
        // std::clog << "sigmaisigmaj(1.0) = " <<
        // ccParam->sigma_i_sigma_j(0,0,1.0) << std::endl;
        // std::clog << "sigmaisigmaj(2.0) = " <<
        // ccParam->sigma_i_sigma_j(0,0,2.0) << std::endl;
        // std::clog << "alphaisigmaj(0.0) = " <<
        // ccParam->alpha_i_sigma_j(0,0,0.0) << std::endl;
        // std::clog << "alphaisigmaj(1.0) = " <<
        // ccParam->alpha_i_sigma_j(0,0,1.0) << std::endl;
        // std::clog << "alphaisigmaj(2.0) = " <<
        // ccParam->alpha_i_sigma_j(0,0,2.0) << std::endl;
        // std::clog << "alphaisigmaj(0.0) = " <<
        // ccParam->alpha_i_sigma_j(1,0,0.0) << std::endl;
        // std::clog << "alphaisigmaj(1.0) = " <<
        // ccParam->alpha_i_sigma_j(1,0,1.0) << std::endl;
        // std::clog << "alphaisigmaj(2.0) = " <<
        // ccParam->alpha_i_sigma_j(1,0,2.0) << std::endl;
        // std::clog << "HiAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_alpha_i_alpha_j(0,0,0.0) << std::endl;
        // std::clog << "HiAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_alpha_i_alpha_j(0,0,1.0) << std::endl;
        // std::clog << "HiAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_alpha_i_alpha_j(0,0,2.0) << std::endl;
        // std::clog << "HiAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_alpha_i_alpha_j(1,1,0.0) << std::endl;
        // std::clog << "HiAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_alpha_i_alpha_j(1,1,1.0) << std::endl;
        // std::clog << "HiAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_alpha_i_alpha_j(1,1,2.0) << std::endl;
        // std::clog << "HiAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_alpha_i_alpha_j(1,0,0.0) << std::endl;
        // std::clog << "HiAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_alpha_i_alpha_j(1,0,1.0) << std::endl;
        // std::clog << "HiAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_alpha_i_alpha_j(1,0,2.0) << std::endl;
        // std::clog << "HiHjAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_H_j_alpha_i_alpha_j(0,0,0.0) << std::endl;
        // std::clog << "HiHjAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_H_j_alpha_i_alpha_j(0,0,1.0) << std::endl;
        // std::clog << "HiHjAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_H_j_alpha_i_alpha_j(0,0,2.0) << std::endl;
        // std::clog << "HiHjAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_H_j_alpha_i_alpha_j(1,1,0.0) << std::endl;
        // std::clog << "HiHjAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_H_j_alpha_i_alpha_j(1,1,1.0) << std::endl;
        // std::clog << "HiHjAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_H_j_alpha_i_alpha_j(1,1,2.0) << std::endl;
        // std::clog << "HiHjAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_H_j_alpha_i_alpha_j(1,0,0.0) << std::endl;
        // std::clog << "HiHjAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_H_j_alpha_i_alpha_j(1,0,1.0) << std::endl;
        // std::clog << "HiHjAlphaIAlphaJ(0.0) = " <<
        // ccParam->H_i_H_j_alpha_i_alpha_j(1,0,2.0) << std::endl;
        // std::clog << "HiAlphaISigmaJ(0.0) = " <<
        // ccParam->H_i_alpha_i_sigma_j(0,0,0.0) << std::endl;
        // std::clog << "HiAlphaISigmaJ(0.0) = " <<
        // ccParam->H_i_alpha_i_sigma_j(0,0,1.0) << std::endl;
        // std::clog << "HiAlphaISigmaJ(0.0) = " <<
        // ccParam->H_i_alpha_i_sigma_j(0,0,2.0) << std::endl;
        // std::clog << "HiAlphaISigmaJ(0.0) = " <<
        // ccParam->H_i_alpha_i_sigma_j(1,0,0.0) << std::endl;
        // std::clog << "HiAlphaISigmaJ(0.0) = " <<
        // ccParam->H_i_alpha_i_sigma_j(1,0,1.0) << std::endl;
        // std::clog << "HiAlphaISigmaJ(0.0) = " <<
        // ccParam->H_i_alpha_i_sigma_j(1,0,2.0) << std::endl;
        // std::clog << "int_alphaialphaj(0.0) = " <<
        // ccParam->int_alpha_i_alpha_j(0,0,0.0,0.0) << std::endl;
        // std::clog << "int_alphaialphaj(1.0) = " <<
        // ccParam->int_alpha_i_alpha_j(0,0,0.0,1.0) << std::endl;
        // std::clog << "int_alphaialphaj(2.0) = " <<
        // ccParam->int_alpha_i_alpha_j(0,0,0.0,2.0) << std::endl;
        // std::clog << "int_alphaialphaj(0.0) = " <<
        // ccParam->int_alpha_i_alpha_j(1,1,0.0,0.0) << std::endl;
        // std::clog << "int_alphaialphaj(1.0) = " <<
        // ccParam->int_alpha_i_alpha_j(1,1,0.0,1.0) << std::endl;
        // std::clog << "int_alphaialphaj(2.0) = " <<
        // ccParam->int_alpha_i_alpha_j(1,1,0.0,2.0) << std::endl;
        // std::clog << "int_alphaialphaj(0.0) = " <<
        // ccParam->int_alpha_i_alpha_j(0,1,0.0,0.0) << std::endl;
        // std::clog << "int_alphaialphaj(1.0) = " <<
        // ccParam->int_alpha_i_alpha_j(0,1,0.0,1.0) << std::endl;
        // std::clog << "int_alphaialphaj(2.0) = " <<
        // ccParam->int_alpha_i_alpha_j(0,1,0.0,2.0) << std::endl;
        // std::clog << "int_alphaialphaj(0.0) = " <<
        // ccParam->int_alpha_i_alpha_j(1,0,0.0,0.0) << std::endl;
        // std::clog << "int_alphaialphaj(1.0) = " <<
        // ccParam->int_alpha_i_alpha_j(1,0,0.0,1.0) << std::endl;
        // std::clog << "int_alphaialphaj(2.0) = " <<
        // ccParam->int_alpha_i_alpha_j(1,0,0.0,2.0) << std::endl;
        // std::clog << "int_sigmaisigmaj(0.0) = " <<
        // ccParam->int_sigma_i_sigma_j(0,0,0.0,0.0) << std::endl;
        // std::clog << "int_sigmaisigmaj(1.0) = " <<
        // ccParam->int_sigma_i_sigma_j(0,0,0.0,1.0) << std::endl;
        // std::clog << "int_sigmaisigmaj(2.0) = " <<
        // ccParam->int_sigma_i_sigma_j(0,0,0.0,2.0) << std::endl;
        // std::clog << "int_alphaisigmaj(0.0) = " <<
        // ccParam->int_alpha_i_sigma_j(0,0,0.0,0.0) << std::endl;
        // std::clog << "int_alphaisigmaj(1.0) = " <<
        // ccParam->int_alpha_i_sigma_j(0,0,0.0,1.0) << std::endl;
        // std::clog << "int_alphaisigmaj(2.0) = " <<
        // ccParam->int_alpha_i_sigma_j(0,0,0.0,2.0) << std::endl;
        // std::clog << "int_alphaisigmaj(0.0) = " <<
        // ccParam->int_alpha_i_sigma_j(1,0,0.0,0.0) << std::endl;
        // std::clog << "int_alphaisigmaj(1.0) = " <<
        // ccParam->int_alpha_i_sigma_j(1,0,0.0,1.0) << std::endl;
        // std::clog << "int_alphaisigmaj(2.0) = " <<
        // ccParam->int_alpha_i_sigma_j(1,0,0.0,2.0) << std::endl;
        // std::clog << "int_H_i_alphaialphaj(0.0) = " <<
        // ccParam->int_H_i_alpha_i_alpha_j(0,0,0.0,0.0) << std::endl;
        // std::clog << "int_H_i_alphaialphaj(1.0) = " <<
        // ccParam->int_H_i_alpha_i_alpha_j(0,0,0.0,1.0) << std::endl;
        // std::clog << "int_H_i_alphaialphaj(2.0) = " <<
        // ccParam->int_H_i_alpha_i_alpha_j(0,0,0.0,2.0) << std::endl;
        // std::clog << "int_H_i_alphaialphaj(0.0) = " <<
        // ccParam->int_H_i_alpha_i_alpha_j(1,1,0.0,0.0) << std::endl;
        // std::clog << "int_H_i_alphaialphaj(1.0) = " <<
        // ccParam->int_H_i_alpha_i_alpha_j(1,1,0.0,1.0) << std::endl;
        // std::clog << "int_H_i_alphaialphaj(2.0) = " <<
        // ccParam->int_H_i_alpha_i_alpha_j(1,1,0.0,2.0) << std::endl;
        // std::clog << "int_H_i_alphaialphaj(0.0) = " <<
        // ccParam->int_H_i_alpha_i_alpha_j(0,1,0.0,0.0) << std::endl;
        // std::clog << "int_H_i_alphaialphaj(1.0) = " <<
        // ccParam->int_H_i_alpha_i_alpha_j(0,1,0.0,1.0) << std::endl;
        // std::clog << "int_H_i_alphaialphaj(2.0) = " <<
        // ccParam->int_H_i_alpha_i_alpha_j(0,1,0.0,2.0) << std::endl;
        // std::clog << "int_H_i_alphaialphaj(0.0) = " <<
        // ccParam->int_H_i_alpha_i_alpha_j(1,0,0.0,0.0) << std::endl;
        // std::clog << "int_H_i_alphaialphaj(1.0) = " <<
        // ccParam->int_H_i_alpha_i_alpha_j(1,0,0.0,1.0) << std::endl;
        // std::clog << "int_H_i_alphaialphaj(2.0) = " <<
        // ccParam->int_H_i_alpha_i_alpha_j(1,0,0.0,2.0) << std::endl;
        // std::clog << "int_H_i_H_j_alphaialphaj(0.0) = " <<
        // ccParam->int_H_i_H_j_alpha_i_alpha_j(0,0,0.0,0.0) << std::endl;
        // std::clog << "int_H_i_H_j_alphaialphaj(1.0) = " <<
        // ccParam->int_H_i_H_j_alpha_i_alpha_j(0,0,0.0,1.0) << std::endl;
        // std::clog << "int_H_i_H_j_alphaialphaj(2.0) = " <<
        // ccParam->int_H_i_H_j_alpha_i_alpha_j(0,0,0.0,2.0) << std::endl;
        // std::clog << "int_H_i_H_j_alphaialphaj(0.0) = " <<
        // ccParam->int_H_i_H_j_alpha_i_alpha_j(1,1,0.0,0.0) << std::endl;
        // std::clog << "int_H_i_H_j_alphaialphaj(1.0) = " <<
        // ccParam->int_H_i_H_j_alpha_i_alpha_j(1,1,0.0,1.0) << std::endl;
        // std::clog << "int_H_i_H_j_alphaialphaj(2.0) = " <<
        // ccParam->int_H_i_H_j_alpha_i_alpha_j(1,1,0.0,2.0) << std::endl;
        // std::clog << "int_H_i_H_j_alphaialphaj(0.0) = " <<
        // ccParam->int_H_i_H_j_alpha_i_alpha_j(0,1,0.0,0.0) << std::endl;
        // std::clog << "int_H_i_H_j_alphaialphaj(1.0) = " <<
        // ccParam->int_H_i_H_j_alpha_i_alpha_j(0,1,0.0,1.0) << std::endl;
        // std::clog << "int_H_i_H_j_alphaialphaj(2.0) = " <<
        // ccParam->int_H_i_H_j_alpha_i_alpha_j(0,1,0.0,2.0) << std::endl;
        // std::clog << "int_H_i_H_j_alphaialphaj(0.0) = " <<
        // ccParam->int_H_i_H_j_alpha_i_alpha_j(1,0,0.0,0.0) << std::endl;
        // std::clog << "int_H_i_H_j_alphaialphaj(1.0) = " <<
        // ccParam->int_H_i_H_j_alpha_i_alpha_j(1,0,0.0,1.0) << std::endl;
        // std::clog << "int_H_i_H_j_alphaialphaj(2.0) = " <<
        // ccParam->int_H_i_H_j_alpha_i_alpha_j(1,0,0.0,2.0) << std::endl;
        // std::clog << "int_H_i_alphaisigmaj(0.0) = " <<
        // ccParam->int_H_i_alpha_i_sigma_j(0,0,0.0,0.0) << std::endl;
        // std::clog << "int_H_i_alphaisigmaj(1.0) = " <<
        // ccParam->int_H_i_alpha_i_sigma_j(0,0,0.0,1.0) << std::endl;
        // std::clog << "int_H_i_alphaisigmaj(2.0) = " <<
        // ccParam->int_H_i_alpha_i_sigma_j(0,0,0.0,2.0) << std::endl;
        // std::clog << "int_H_i_alphaisigmaj(0.0) = " <<
        // ccParam->int_H_i_alpha_i_sigma_j(1,0,0.0,0.0) << std::endl;
        // std::clog << "int_H_i_alphaisigmaj(1.0) = " <<
        // ccParam->int_H_i_alpha_i_sigma_j(1,0,0.0,1.0) << std::endl;
        // std::clog << "int_H_i_alphaisigmaj(2.0) = " <<
        // ccParam->int_H_i_alpha_i_sigma_j(1,0,0.0,2.0) << std::endl;

        // std::clog << "rho alpha-alpha 00" << ccParam->rho_alpha_alpha(0,0) <<
        // std::endl;
        // std::clog << "rho alpha-alpha 01" << ccParam->rho_alpha_alpha(0,1) <<
        // std::endl;
        // std::clog << "rho alpha-alpha 10" << ccParam->rho_alpha_alpha(1,0) <<
        // std::endl;
        // std::clog << "rho alpha-alpha 11" << ccParam->rho_alpha_alpha(1,1) <<
        // std::endl;
        // std::clog << "rho alpha-sigma 00" << ccParam->rho_alpha_sigma(0,0) <<
        // std::endl;
        // std::clog << "rho alpha-sigma 10" << ccParam->rho_alpha_sigma(1,0) <<
        // std::endl;
        // std::clog << "rho sigma-sigma 00" << ccParam->rho_sigma_sigma(0,0) <<
        // std::endl;

        // end test parametrization

        boost::shared_ptr<
            CcLgmProcess<detail::CcLgmPiecewise, detail::LgmFxPiecewiseSigma,
                         detail::LgmPiecewiseAlphaConstantKappa> > process =
            boost::make_shared<CcLgmProcess<
                detail::CcLgmPiecewise, detail::LgmFxPiecewiseSigma,
                detail::LgmPiecewiseAlphaConstantKappa> >(ccParam, fxSpots,
                                                          curves);

        // generate paths

        Size n = atoi(getenv("N"));               // N paths
        Size steps = 1.0 * atof(getenv("STEPS")); // STEPS steps per year
        TimeGrid grid(1.0, steps);

        PseudoRandom::rsg_type sg =
            PseudoRandom::make_sequence_generator(steps * 3, 14);
        MultiPathGenerator<PseudoRandom::rsg_type> pg(process, grid, sg, false);

        std::vector<Sample<MultiPath> > paths;
        for (Size j = 0; j < n; ++j) {
            paths.push_back(pg.next());
        }

        // output paths for visual inspection in gnuplot

        for (Size i = 0; i < paths[0].value[0].length(); ++i) {
            std::cout << grid[i] << " ";
            for (Size j = 0; j < n; ++j) {
                std::cout << std::exp(paths[j].value[0][i]) << " "
                          << paths[j].value[1][i] << " " << paths[j].value[2][i]
                          << " ";
            }
            std::cout << "\n";
        }

        // test: 1 USD in 1y, priced in domestic measure

        Size l = paths[0].value[0].length() - 1;
        IncrementalStatistics stat;
        for (Size j = 0; j < n; ++j) {
            Real fx = std::exp(paths[j].value[0][l]);
            Real zeur = paths[j].value[1][l];
            Real zusd = paths[j].value[2][l];
            Real y =
                (zeur - eurLgm->stateProcess()->expectation(0.0, 0.0, 1.0)) /
                eurLgm->stateProcess()->stdDeviation(0.0, 0.0, 1.0);
            stat.add(1.0 * fx / eurLgm->numeraire(1.0,y));
        }
        std::clog << "1 USD @ 1y  = " << stat.mean() << " EUR +/- "
                  << stat.errorEstimate() << std::endl;
        ;
        std::clog << "curve price = " << usdYts->discount(1.0) << " spot "
                  << std::exp(fxSpots[0]->value()) << " EUR price "
                  << usdYts->discount(1.0) * std::exp(fxSpots[0]->value())
                  << "\n";

        return 0;

    } catch (QuantLib::Error e) {
        std::clog << "ql exception : " << e.what() << "\n";
    } catch (std::exception e) {
        std::clog << "std exception: " << e.what() << "\n";
    }
}
