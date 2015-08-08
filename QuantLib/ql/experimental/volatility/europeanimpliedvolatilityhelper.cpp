/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2014 Klaus Spanderen

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

/*! \file europeanimpliedvolatility.cpp
*/

#include <ql/math/functional.hpp>
#include <ql/math/distributions/normaldistribution.hpp>
#include <ql/experimental/volatility/europeanimpliedvolatilityhelper.hpp>

#include <boost/math/special_functions/erf.hpp>

#include <iostream>

namespace QuantLib {

	EuropeanImpliedVolatilityHelper::EuropeanImpliedVolatilityHelper(
		Option::Type type,
		Real forward, Real strike, DiscountFactor discount, Time maturity)
	: theta_     ((type == Option::Call) ? 1.0 : -1.0),
	  x_	     (std::log(forward/strike)),
	  forward_   (forward),
	  strike_    (strike),
	  discount_  (discount),
	  maturity_  (maturity),
	  iterations_(0u) {	}

	Volatility EuropeanImpliedVolatilityHelper::calculate(
		Real price, Real tol, Size maxEvaluations) const {
		iterations_ = 0u;

		QL_REQUIRE(price > 0.0, "price must be create than zero");

		if (std::fabs(x_) < QL_EPSILON) {
			return -2*invN(0.5*(1-normalizedPrice(price)))
					/std::sqrt(maturity_);
		}
		const bool otm   = theta_*x_ < 0.0;
		const Real theta = otm ? theta_ : -theta_;

		const Real sp = normalizedPrice(price);
		const Real intrinsic = (otm) ? 0.0 : 2*theta_*std::sinh(0.5*x_);
		const Real beta   = sp - intrinsic;

		QL_REQUIRE(otm || (sp - intrinsic)/sp > 1e4*QL_EPSILON,
				   "deep ITM option given and option price is "
				<< "too close to the intrinsic value of the option."
				<< std::endl
				<< "normalized price : " << sp
				<< std::endl
				<< "price - intrinsic: " << sp - intrinsic);

		const Volatility stdDev_c = std::sqrt(2*std::fabs(x_));
		const Real b_c = normalizedPrice(x_, stdDev_c, theta);

		Real stdDev;
		if (beta < b_c) {
			const Volatility stdDev_s = stdDevHigh(x_, 0.0, theta, b_c);
			const Real b_s = normalizedPrice(x_, stdDev_s, theta);
			const Volatility stdDev_ls = stdDevLow(x_, b_s, b_c);
			const Volatility stdDev_hs = stdDevHigh(x_, b_s, theta, b_c);

			const Real w_s =
				std::pow(std::min(std::max( (stdDev_s - stdDev_ls)
										   /(stdDev_hs - stdDev_ls), 0.0),
								  1.0), std::log(b_c/beta)/std::log(b_c/b_s));

			const Volatility stdDev_l = stdDevLow(x_, beta, b_c);
			stdDev = (stdDev_s < stdDev_ls)
				? stdDev_l
				: (1-w_s)*stdDev_l + w_s*stdDevHigh(x_, beta, theta, b_c);
		}
		else {
			stdDev = stdDevHigh(x_, beta, theta, b_c);
		}

//		Real nu;
//		do {
//			QL_REQUIRE(iterations_++ < maxEvaluations,
//				"maximum number of function evaluations ("
//                << maxEvaluations << ") exceeded");
//
//			const Real b = normalizedPrice(x_, stdDev, theta);
//			const Real bs = std::exp(-0.5*square<Real>()(x_/stdDev)
//			                         -0.5*square<Real>()(0.5*stdDev))
//								*M_1_SQRTPI*M_SQRT1_2;
//
//			nu =  std::max(-0.5*stdDev, (beta < b_c)
//				? std::log(beta/b)*std::log(b)/std::log(beta)*b/bs
//				: (beta-b)/bs);
//
//			const Real eta = std::max(-0.75,
//				0.5*nu*(square<Real>()(x_/stdDev)/stdDev - 0.25*stdDev
//						- ((beta < b_c) ? (2+std::log(b))/std::log(b)*bs/b
//									    : 0.0)));
//
//			stdDev += std::max(nu/(1+eta), -0.5*stdDev);
//		} while (std::fabs(nu/stdDev) > tol);

		return stdDev/std::sqrt(maturity_);
	}

	Size EuropeanImpliedVolatilityHelper::iterations() const {
		return iterations_;
	}

	Real EuropeanImpliedVolatilityHelper::blackPrice(Volatility vol)
	const {
		return normalizedPrice(x_, vol*std::sqrt(maturity_), theta_)
				*std::sqrt(forward_*strike_)*discount_;
	}

	Real EuropeanImpliedVolatilityHelper::normalizedPrice(
		Real x, Volatility stdDev, Real theta) const {

		const Real phi = std::exp(0.5*x);
		if (theta > 0 && x <= 0) {
			// OTM Call
			const Real h = x/stdDev;
			const Real t = 0.5*stdDev;

			const static Real tSmall = 2*std::pow(QL_EPSILON, 1/16.);
			const Real asymExpansionThreshold = 10;
			if (   (std::fabs(h) > asymExpansionThreshold)
				&& (t < std::fabs(h) - asymExpansionThreshold + tSmall)) {

			    const Real e=(t/h)*(t/h), r=((h+t)*(h-t)), q=(h/r)*(h/r);

			    // This expansion is taken from Peter Jaeckel's reference implementation,
				// http://www.pjaeckel.webspace.virginmedia.com/LetsBeRational.7z
			    const Real asymExpansionSum = (2.0+q*(-6.0E0-2.0*e+3.0*q*(1.0E1+e*(2.0E1+2.0*e)+5.0*q*(-1.4E1+e*(-7.0E1+e*(-4.2E1-2.0*e))
			    	+7.0*q*(1.8E1+e*(1.68E2+e*(2.52E2+e*(7.2E1+2.0*e)))+9.0*q*(-2.2E1+e*(-3.3E2+e*(-9.24E2
			    	+e*(-6.6E2+e*(-1.1E2-2.0*e))))+1.1E1*q*(2.6E1+e*(5.72E2+e*(2.574E3+e*(3.432E3+e*(1.43E3
			    	+e*(1.56E2+2.0*e)))))+1.3E1*q*(-3.0E1+e*(-9.1E2+e*(-6.006E3+e*(-1.287E4
			    	+e*(-1.001E4+e*(-2.73E3+e*(-2.1E2-2.0*e))))))+1.5E1*q*(3.4E1+e*(1.36E3+e*(1.2376E4+e*(3.8896E4
			    	+e*(4.862E4+e*(2.4752E4+e*(4.76E3+e*(2.72E2+2.0*e)))))))+1.7E1*q*(-3.8E1+e*(-1.938E3+e*(-2.3256E4
			    	+e*(-1.00776E5+e*(-1.84756E5+e*(-1.51164E5+e*(-5.4264E4+e*(-7.752E3+e*(-3.42E2-2.0*e))))))))
			    	+1.9E1*q*(4.2E1+e*(2.66E3+e*(4.0698E4+e*(2.3256E5+e*(5.8786E5+e*(7.05432E5+e*(4.0698E5+e*(1.08528E5
			    	+e*(1.197E4+e*(4.2E2+2.0*e)))))))))+2.1E1*q*(-4.6E1+e*(-3.542E3+e*(-6.7298E4+e*(-4.90314E5+e*(-1.63438E6
			    	+e*(-2.704156E6+e*(-2.288132E6+e*(-9.80628E5+e*(-2.01894E5+e*(-1.771E4+e*(-5.06E2-2.0*e))))))))))+2.3E1*q*(5.0E1
			    	+e*(4.6E3+e*(1.0626E5+e*(9.614E5+e*(4.08595E6+e*(8.9148E6+e*(1.04006E7+e*(6.53752E6+e*(2.16315E6+e*(3.542E5
			    	+e*(2.53E4+e*(6.0E2+2.0*e)))))))))))+2.5E1*q*(-5.4E1+e*(-5.85E3+e*(-1.6146E5+e*(-1.77606E6+e*(-9.37365E6
			    	+e*(-2.607579E7+e*(-4.01166E7+e*(-3.476772E7+e*(-1.687257E7+e*(-4.44015E6+e*(-5.9202E5+e*(-3.51E4
			    	+e*(-7.02E2-2.0*e))))))))))))+2.7E1*q*(5.8E1+e*(7.308E3+e*(2.3751E5+e*(3.12156E6+e*(2.003001E7
			    	+e*(6.919458E7+e*(1.3572783E8+e*(1.5511752E8+e*(1.0379187E8+e*(4.006002E7+e*(8.58429E6
			    	+e*(9.5004E5+e*(4.7502E4+e*(8.12E2+2.0*e)))))))))))))+2.9E1*q*(-6.2E1+e*(-8.99E3+e*(-3.39822E5+e*(-5.25915E6
			        +e*(-4.032015E7+e*(-1.6934463E8+e*(-4.1250615E8+e*(-6.0108039E8+e*(-5.3036505E8+e*(-2.8224105E8
			        +e*(-8.870433E7+e*(-1.577745E7+e*(-1.472562E6+e*(-6.293E4+e*(-9.3E2-2.0*e))))))))))))))
			        +3.1E1*q*(6.6E1+e*(1.0912E4+e*(4.74672E5+e*(8.544096E6+e*(7.71342E7+e*(3.8707344E8+e*(1.14633288E9
			        +e*(2.07431664E9+e*(2.33360622E9+e*(1.6376184E9+e*(7.0963464E8+e*(1.8512208E8+e*(2.7768312E7
			        +e*(2.215136E6+e*(8.184E4+e*(1.056E3+2.0*e)))))))))))))))+3.3E1*(-7.0E1+e*(-1.309E4+e*(-6.49264E5+e*(-1.344904E7
			        +e*(-1.4121492E8+e*(-8.344518E8+e*(-2.9526756E9+e*(-6.49588632E9+e*(-9.0751353E9+e*(-8.1198579E9+e*(
			        -4.6399188E9+e*(-1.6689036E9+e*(-3.67158792E8+e*(-4.707164E7+e*(-3.24632E6+e*(-1.0472E5+e*(
			        -1.19E3-2.0*e)))))))))))))))))*q)))))))))))))))));

			    const Real b = M_SQRT_2*M_1_SQRTPI
			    	*exp((-0.5*(h*h+t*t)))*(t/r)*asymExpansionSum;

			    return std::max(b , 0.);
			}
			else if (t < tSmall) {
				const Real w  = square<Real>()(t);
				const Real h2 = square<Real>()(h);
				const Real a = 1+h*(0.5*M_SQRTPI*M_SQRT2)*erfcx(-M_SQRT_2*h);

				// This expansion is taken from Peter Jaeckel's reference implementation,
				// http://www.pjaeckel.webspace.virginmedia.com/LetsBeRational.7z
				const Real e = 2*t*(a+w*((-1+3*a+a*h2)/6+w*((-7+15*a+h2*(-1+10*a+a*h2))/120
					+w*((-57+105*a+h2*(-18+105*a+h2*(-1+21*a+a*h2)))/5040
					+w*((-561+945*a+h2*(-285+1260*a+h2*(-33+378*a+h2*(-1+36*a+a*h2))))/362880
					+w*((-6555+10395*a+h2*(-4680+17325*a+h2*(-840+6930*a+h2*(-52+990*a
					+h2*(-1+55*a+a*h2)))))/39916800+((-89055+135135*a+h2*(-82845+270270*a
					+h2*(-20370+135135*a+h2*(-1926+25740*a+h2*(-75+2145*a
					+h2*(-1+78*a+a*h2))))))*w)/6227020800.0))))));

				const Real b = M_SQRT_2*M_1_SQRTPI*std::exp(-0.5*(h2+w))*e;
				return std::max(b, 0.0);
			}
			else if (t > 0.85 + std::fabs(h)) {
				return phi * N(x/stdDev + 0.5*stdDev)
						   - N(x/stdDev - 0.5*stdDev) / phi;
			}
			else {
				return M_SQRT_2*M_1_SQRTPI*std::exp(-0.5*(h*h+t*t))
						*(Y(h+t) - Y(h-t));
			}
		}
		else if (theta > 0 && x > 0) {
			return normalizedPrice(-x, stdDev, theta) + (phi-1/phi);
		}
		else if (theta < 0 && x > 0) {
			return normalizedPrice(-x, stdDev, -theta);
		}
		else {
			return normalizedPrice(x, stdDev, -theta) + (1/phi-phi);
		}
	}

	Real EuropeanImpliedVolatilityHelper::normalizedPrice(Real price)
	const {
		return price/(discount_*std::sqrt(forward_*strike_));
	}

	Volatility EuropeanImpliedVolatilityHelper::stdDevLow(
		Real x, Real beta, Real b_c) {
		return std::sqrt(2*x*x/(std::fabs(x) - 4*std::log(beta/b_c)));
	}

	Volatility EuropeanImpliedVolatilityHelper::stdDevHigh(
		Real x, Real beta, Real theta, Real b_c) {
		return -2*invN( (std::exp(0.5*theta*x)-beta)
				   	   /(std::exp(0.5*theta*x) - b_c)
				   	   *N(-std::sqrt(0.5*std::fabs(x))));
	}

	Real EuropeanImpliedVolatilityHelper::N(Real x) {
		return MaddockCumulativeNormal()(x);
	}

	Real EuropeanImpliedVolatilityHelper::Y(Real x) {
		return 0.5*M_SQRTPI*M_SQRT2*erfcx(-M_SQRT_2*x);
	}

	Real EuropeanImpliedVolatilityHelper::erfcx(Real x) {

		if (x < 25.0) {
			return std::exp(x*x)*boost::math::erfc(x);
		}
		else {
			const Real y = 1. / x;
			const Real z = square<Real>()(y);
			return 0.564189583547756287
				*y*(1.+z*(-0.5+z*(0.75+z*(-1.875+z*(6.5625-29.53125*z)))));
		}
	}

	Real EuropeanImpliedVolatilityHelper::invN(Real x) {
		return MaddockInverseCumulativeNormal()(x);
	}
}
