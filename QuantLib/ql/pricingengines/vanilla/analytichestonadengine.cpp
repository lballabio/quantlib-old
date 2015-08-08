/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2015 Klaus Spanderen

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

/*! \file analytichestonadengine.hpp
  \brief analytic pricing engine for a heston option
  based on fourier transformation
*/

#include <ql/math/functional.hpp>
#include <ql/instruments/payoffs.hpp>
#include <ql/math/integrals/gaussianadquadratures.hpp>
#include <ql/pricingengines/vanilla/analytichestonadengine.hpp>

#include <boost/assign/std/vector.hpp>
using namespace boost::assign;

namespace QuantLib {

	namespace {
		std::complex<CppAD::AD<Real> > operator*(
			Real a, const std::complex<CppAD::AD<Real> >& b ) {
			return std::complex<CppAD::AD<Real> >(a*b.real(), a*b.imag());
		}
		std::complex<CppAD::AD<Real> > operator*(
			const std::complex<CppAD::AD<Real> >& b, Real a) {
			return a*b;
		}
		std::complex<CppAD::AD<Real> > operator-(
			Real a, const std::complex<CppAD::AD<Real> >& b) {
			return std::complex<CppAD::AD<Real> >(a - b.real(), -b.imag());
		}
	}

    // helper class for integration
    class AnalyticHestonADEngine::Fj_Helper
        : public std::unary_function<Real, Real>
    {
    public:
        Fj_Helper(
        	CppAD::AD<Real>& kappa,
        	CppAD::AD<Real>& theta,
        	CppAD::AD<Real>& sigma,
        	CppAD::AD<Real>& v0,
        	CppAD::AD<Real>& s0,
        	CppAD::AD<Real>& rho,
            const AnalyticHestonADEngine* const engine,
            ComplexLogFormula cpxLog,
            Time term,
            Real strike,
            Real ratio,
            Size j);

        CppAD::AD<Real> operator()(Real phi)      const;

    private:
        const Size j_;
        const CppAD::AD<Real> &kappa_, &theta_, &sigma_, &v0_;
        const ComplexLogFormula cpxLog_;

        // helper variables
        const Time term_;
        CppAD::AD<Real> x_;
        const Real sx_;
        CppAD::AD<Real> dd_;
        const CppAD::AD<Real> sigma2_, rsigma_;
        const CppAD::AD<Real> t0_;

        // log branch counter
        mutable int  b_;     // log branch counter
        mutable Real g_km1_; // imag part of last log value

        const AnalyticHestonADEngine* const engine_;
    };



    AnalyticHestonADEngine::Fj_Helper::Fj_Helper(
		CppAD::AD<Real>& kappa,
		CppAD::AD<Real>& theta,
		CppAD::AD<Real>& sigma,
		CppAD::AD<Real>& v0,
		CppAD::AD<Real>& s0,
		CppAD::AD<Real>& rho,
        const AnalyticHestonADEngine* const engine,
        ComplexLogFormula cpxLog,
        Time term,
        Real strike,
        Real ratio,
        Size j)
        :
        j_(j),
        kappa_(kappa),
        theta_(theta),
        sigma_(sigma),
        v0_(v0),
        cpxLog_(cpxLog),
        term_(term),
        x_(log(s0)),
        sx_(std::log(strike)),
        dd_(x_-std::log(ratio)),
        sigma2_(sigma_*sigma_),
        rsigma_(rho*sigma_),
        t0_(kappa - ((j== 1)? rho*sigma : 0)),
        b_(0),
        g_km1_(0),
        engine_(engine) {
    }


    CppAD::AD<Real> AnalyticHestonADEngine::Fj_Helper::operator()(
    	Real phi) const {

        const CppAD::AD<Real> rpsig(rsigma_*phi);

        const std::complex<CppAD::AD<Real> > t1 =
        	t0_+std::complex<CppAD::AD<Real> >(0, -rpsig);
        const std::complex<CppAD::AD<Real> > d =
            std::sqrt(t1*t1 - sigma2_*phi
                      *std::complex<CppAD::AD<Real> >(-phi, (j_== 1)? 1 : -1));
        const std::complex<CppAD::AD<Real> > ex = exp(-d*term_);

        if (cpxLog_ == Gatheral) {
            if (phi != 0.0) {
                if (sigma_ > 1e-5) {
                    const std::complex<CppAD::AD<Real> > p = (t1-d)/(t1+d);
                    const std::complex<CppAD::AD<Real> > g
                        = log((1.0 - p*ex)/(1.0 - p));

                    const std::complex<CppAD::AD<Real> > c(
                    	v0_*(t1-d)*(1.0-ex)/(sigma2_*(1.0-ex*p))
                    	+ (kappa_*theta_)/sigma2_*((t1-d)*term_-2.0*g));

                    CppAD::AD<Real> im(c.imag()), re(c.real());

                    return
                        exp(std::complex<CppAD::AD<Real> >(re, im)
							 + std::complex<CppAD::AD<Real> >(0.0, phi*(dd_-sx_))
							 ).imag()/phi;
                }
                else {
                	QL_FAIL("ouch, sigma < 1e-5");
                }
            }
            else {
            	QL_FAIL("ouch, phi = 0");
            }
        }
        else if (cpxLog_ == BranchCorrection) {
        	QL_FAIL("ouch, BranchCorrection");
        }
        else {
            QL_FAIL("unknown complex logarithm formula");
        }
    }

    AnalyticHestonADEngine::AnalyticHestonADEngine(
                              const boost::shared_ptr<HestonModel>& model,
                              Size integrationOrder)
    : GenericModelEngine<HestonModel,
                         VanillaOption::arguments,
                         VanillaOption::results>(model),
      evaluations_(0),
      cpxLog_     (Gatheral),
      integration_(new Integration(
                          Integration::gaussLaguerre(integrationOrder))) {
    }


    Size AnalyticHestonADEngine::numberOfEvaluations() const {
        return evaluations_;
    }

    void AnalyticHestonADEngine::doCalculation(Real riskFreeDiscount,
                                             Real dividendDiscount,
                                             Real spotPrice,
                                             Real strikePrice,
                                             Real term,
                                             Real kappa, Real theta, Real sigma, Real v0, Real rho,
                                             const TypePayoff& type,
                                             const Integration& integration,
                                             const ComplexLogFormula cpxLog,
                                             const AnalyticHestonADEngine* const enginePtr,
                                             VanillaOption::results& results,
                                             Size& evaluations)
    {
        std::vector<CppAD::AD<Real> > params;
        params += spotPrice, v0, kappa, theta, sigma, rho;
        CppAD::Independent(params);

        std::vector<Real> vp;
        vp += spotPrice, v0, kappa, theta, sigma, rho;
        const Real ratio = riskFreeDiscount/dividendDiscount;

        evaluations = 0;

        const CppAD::AD<Real> p1 = integration.calculate(
            Fj_Helper(params[2], params[3], params[4],
            		  params[1], params[0], params[5], enginePtr,
                      cpxLog, term, strikePrice, ratio, 1))/M_PI;
        evaluations+= integration.numberOfEvaluations();

        const CppAD::AD<Real> p2 = integration.calculate(
            Fj_Helper(params[2], params[3], params[4],
            		  params[1], params[0], params[5], enginePtr,
                      cpxLog, term, strikePrice, ratio, 2))/M_PI;
        evaluations+= integration.numberOfEvaluations();

        std::vector<CppAD::AD<Real> > y(1);

        switch (type.optionType())
        {
          case Option::Call:
            y[0] = params[0]*dividendDiscount*(p1+0.5)
            	- strikePrice*riskFreeDiscount*(p2+0.5);
            break;
          case Option::Put:
        	  y[0] = params[0]*dividendDiscount*(p1-0.5)
                - strikePrice*riskFreeDiscount*(p2-0.5);
            break;
          default:
            QL_FAIL("unknown option type");
        }

        std::vector<Real> moreResults
        	= CppAD::ADFun<Real>(params, y).Reverse(1, std::vector<Real>(1, 1.0));

        results.value = CppAD::Value(y[0]);

        results.delta = moreResults[0];
        results.additionalResults["delta"] = moreResults[0];
        results.additionalResults["v0"]    = moreResults[1];
        results.additionalResults["kappa"] = moreResults[2];
        results.additionalResults["theta"] = moreResults[3];
        results.additionalResults["sigma"] = moreResults[4];
        results.additionalResults["rho"]   = moreResults[5];
    }

    void AnalyticHestonADEngine::calculate() const
    {
        // this is a european option pricer
        QL_REQUIRE(arguments_.exercise->type() == Exercise::European,
                   "not an European option");

        // plain vanilla
        boost::shared_ptr<PlainVanillaPayoff> payoff =
            boost::dynamic_pointer_cast<PlainVanillaPayoff>(arguments_.payoff);
        QL_REQUIRE(payoff, "non plain vanilla payoff given");

        const boost::shared_ptr<HestonProcess>& process = model_->process();

        const Real riskFreeDiscount = process->riskFreeRate()->discount(
                                            arguments_.exercise->lastDate());
        const Real dividendDiscount = process->dividendYield()->discount(
                                            arguments_.exercise->lastDate());

        const Real spotPrice = process->s0()->value();
        QL_REQUIRE(spotPrice > 0.0, "negative or null underlying given");

        const Real strikePrice = payoff->strike();
        const Real term = process->time(arguments_.exercise->lastDate());

        doCalculation(riskFreeDiscount,
                      dividendDiscount,
                      spotPrice,
                      strikePrice,
                      term,
                      model_->kappa(),
                      model_->theta(),
                      model_->sigma(),
                      model_->v0(),
                      model_->rho(),
                      *payoff,
                      *integration_,
                      cpxLog_,
                      this,
                      results_,
                      evaluations_);
    }



    AnalyticHestonADEngine::Integration::Integration(
            Algorithm intAlgo,
            const boost::shared_ptr<GaussianADQuadrature>& gaussianQuadrature)
    : intAlgo_(intAlgo),
      gaussianQuadrature_(gaussianQuadrature) { }


    AnalyticHestonADEngine::Integration
    AnalyticHestonADEngine::Integration::gaussLaguerre(Size intOrder) {
        QL_REQUIRE(intOrder <= 192, "maximum integraton order (192) exceeded");
        return Integration(GaussLaguerre,
                           boost::shared_ptr<GaussianADQuadrature>(
                               new GaussLaguerreADIntegration(intOrder)));
    }

    AnalyticHestonADEngine::Integration
    AnalyticHestonADEngine::Integration::gaussLegendre(Size intOrder) {
        return Integration(GaussLegendre,
                           boost::shared_ptr<GaussianADQuadrature>(
                               new GaussLegendreADIntegration(intOrder)));
    }

    AnalyticHestonADEngine::Integration
    AnalyticHestonADEngine::Integration::gaussChebyshev(Size intOrder) {
        return Integration(GaussChebyshev,
                           boost::shared_ptr<GaussianADQuadrature>(
                               new GaussChebyshevADIntegration(intOrder)));
    }

    AnalyticHestonADEngine::Integration
    AnalyticHestonADEngine::Integration::gaussChebyshev2nd(Size intOrder) {
        return Integration(GaussChebyshev2nd,
                           boost::shared_ptr<GaussianADQuadrature>(
                               new GaussChebyshev2ndADIntegration(intOrder)));
    }

    Size AnalyticHestonADEngine::Integration::numberOfEvaluations() const {
    	return gaussianQuadrature_->order();
    }

    CppAD::AD<Real> AnalyticHestonADEngine::Integration::calculate(
	    const boost::function<CppAD::AD<Real>(Real)>& f) const {

    	CppAD::AD<Real> retVal;

        switch(intAlgo_) {
          case GaussLaguerre:
            retVal = gaussianQuadrature_->operator()(f);
            break;
          default:
              QL_FAIL("unknwon integration algorithm");
        }

        return retVal;
     }
}
