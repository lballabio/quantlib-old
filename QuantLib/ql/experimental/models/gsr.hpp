/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/*
 Copyright (C) 2013 Peter Caspers

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

/*! \file gsr.hpp
    \brief GSR 1 factor model
*/

// uncomment to enably high precision floating point
//#define GSR_ENABLE_NTL

#ifndef quantlib_gsr_hpp
#define quantlib_gsr_hpp

#include <ql/mathconstants.hpp>
#include <ql/models/model.hpp>
#include <ql/models/parameter.hpp>
#include <ql/math/interpolation.hpp>
#include <ql/indexes/iborindex.hpp>
#include <ql/indexes/swapindex.hpp>
#include <ql/time/schedule.hpp>
#include <ql/instruments/vanillaswap.hpp>
#include <ql/math/interpolations/cubicinterpolation.hpp>
#include <ql/math/integrals/simpsonintegral.hpp>
#include <ql/math/integrals/gausslobattointegral.hpp>
#include <ql/math/distributions/normaldistribution.hpp>

#include <ql/experimental/models/gsrprocess.hpp>

#include <boost/math/special_functions.hpp>

#ifdef GSR_ENABLE_NTL
    #include <boost/math/bindings/rr.hpp>
#endif


namespace QuantLib {

    //! One factor gsr model, formulation is in forward measure with time initially taken to be the last date of the yieldTermStructure, but this can be changed to $T$ by forwardMeasureTime(Time T)
	//  notice that $y$ denotes the standardized version of $x$, i.e. $x$ translated by negative expectation and divided by standard deviation. This $y$ should also not be confused with the $y$ from the gsrprocess class
	//  The implementation here shares a lot of code with markovfunctional.xpp. Both models could be based on a common basis.

    class Gsr : public TermStructureConsistentModel, public CalibratedModel, public LazyObject {

      public:

		// constant mean reversion
		Gsr(const Handle<YieldTermStructure>& termStructure,
						const std::vector<Date>& volstepdates,
						const std::vector<Real>& volatilities,
						const Real reversion,
						const Real T = 60.0);
		// piecewise mean reversion (with same step dates as volatilities)
		Gsr(const Handle<YieldTermStructure>& termStructure,
						const std::vector<Date>& volstepdates,
						const std::vector<Real>& volatilities,
						const std::vector<Real>& reversions,
						const Real T = 60.0);
		// constant mean reversion (calibrated), the first dummy parameter indicates this (can be set to true or false, does not matter)
		Gsr(const bool dummy, const Handle<YieldTermStructure>& termStructure,
						const std::vector<Date>& volstepdates,
						const std::vector<Real>& volatilities,
						const Real reversion,
						const Real T = 60.0);
		// piecewise mean reversion (with same step dates as volatilities, calibrated)
		Gsr(const bool dummy, const Handle<YieldTermStructure>& termStructure,
						const std::vector<Date>& volstepdates,
						const std::vector<Real>& volatilities,
						const std::vector<Real>& reversions,
						const Real T = 60.0);
		
		// spreads to adjust forward and discounting curves can be supplied, they are added in a static manner to the model curve, if given these spreads are expected as continuous zero rates 

		const Real zerobond(const Time T, const Time t=0.0, const Real y=0.0, boost::shared_ptr<Interpolation> discountSpread = boost::shared_ptr<Interpolation>()) const;
		const Real zerobond(const Date& maturity, const Date& referenceDate = Null<Date>(), const Real y=0.0, boost::shared_ptr<Interpolation> discountSpread = boost::shared_ptr<Interpolation>()) const;

		const Real numeraire(const Time t, const Real y=0.0, boost::shared_ptr<Interpolation> discountSpread = boost::shared_ptr<Interpolation>()) const;

		// zero bond option pricing via analytical formula (only for monocurve setup)
		const Real zerobondOption(const Option::Type& type, const Date& expiry, const Date& valueDate, const Date& maturity, const Rate strike, const Date& referenceDate = Null<Date>(), const Real y=0.0);

		// zero bond option pricing via numerical integration (only for monocurve setup, TODO extend this to the case value date not euqal expiry date)
		const Real zerobondOption(const Option::Type& type, const Date& expiry, const Date& maturity, const Rate strike, const Date& referenceDate = Null<Date>(), const Real y=0.0,
			boost::shared_ptr<Interpolation> discountSpread = boost::shared_ptr<Interpolation>(),
			const Size yGridPoints=32, const Real yStdDevs=7.0, const bool extrapolatePayoff=true, const bool flatPayoffExtrapolation=false) const;

		const Real forwardRate(const Date& fixing, boost::shared_ptr<IborIndex> iborIdx, const Date& referenceDate = Null<Date>(), const Real y=0.0, boost::shared_ptr<Interpolation> forwardSpread = boost::shared_ptr<Interpolation>()) const;
		const Real swapRate(const Date& fixing, const Period& tenor, boost::shared_ptr<SwapIndex> swapIdx, const Date& referenceDate = Null<Date>(), const Real y=0.0, boost::shared_ptr<Interpolation> forwardSpread = boost::shared_ptr<Interpolation>(), boost::shared_ptr<Interpolation> discountSpread = boost::shared_ptr<Interpolation>()) const;
		const Real swapAnnuity(const Date& fixing, const Period& tenor, boost::shared_ptr<SwapIndex> swapIdx, const Date& referenceDate = Null<Date>(), const Real y=0.0, boost::shared_ptr<Interpolation> discountSpread = boost::shared_ptr<Interpolation>()) const;

		// monocurve setup only
		const Real capletPrice(const Option::Type& type, const Date& expiry, const Rate strike, boost::shared_ptr<IborIndex> iborIdx, const Date& referenceDate = Null<Date>(), const Real y=0.0,
			const Size yGridPoints=32, const Real yStdDevs=7.0, const bool extrapolatePayoff=true, const bool flatPayoffExtrapolation=false) const;
		// monocurve setup only
		const Real swaptionPrice(const Option::Type& type, const Date& expiry, const Period& tenor, const Rate strike, boost::shared_ptr<SwapIndex> swapIdx, const Date& referenceDate = Null<Date>(), const Real y=0.0,
			const Size yGridPoints=32, const Real yStdDevs=7.0, const bool extrapolatePayoff=true, const bool flatPayoffExtrapolation=false) const;

		const Disposable<Array> yGrid(const Real yStdDevs, const int gridPoints, const Real T=1.0, const Real t=0, const Real y=0) const;

		const Real forwardMeasureTime() const { return stateProcess_->getForwardMeasureTime(); }
		const Real forwardMeasureTime(const Real T) const { stateProcess_->setForwardMeasureTime(T); calculate(); }

		const boost::shared_ptr<GsrProcess> stateProcess() const { return stateProcess_; }

		void update() {
			LazyObject::update();
		}

		void performCalculations() const {
			// nothing to do at the moment ... this is really a lazy object
		}

		/*! Computes the integral
        \f[ {2\pi}^{-0.5} \int_{a}^{b} p(x) \exp{-0.5*x*x} \mathrm{d}x \f]
        with
		\f[ p(x) = ax^4+bx^3+cx^2+dx+e \f].
		*/
		const Real gaussianPolynomialIntegral(const Real a, const Real b, const Real c, const Real d, const Real e, const Real x0, const Real x1) const;
		
		/*! Computes the integral
        \f[ {2\pi}^{-0.5} \int_{a}^{b} p(x) \exp{-0.5*x*x} \mathrm{d}x \f]
        with
		\f[ p(x) = a(x-h)^4+b(x-h)^3+c(x-h)^2+d(x-h)+e \f].
		*/
		const Real gaussianShiftedPolynomialIntegral(const Real a, const Real b, const Real c, const Real d, const Real e, const Real h, const Real x0, const Real x1) const;

      protected:
        
		void generateArguments() {
			calculate();
			stateProcess()->flushCache();
			notifyObservers();
		}

      private:

  		void initialize(Real);

		const bool calibrateReversion_;

        NullParameter dummyParameter_;

		Parameter& reversion_;
		Parameter reversionNc_; // this is used if reversion is not calibrated
		Parameter& sigma_;
		
		std::vector<Date> volstepdates_; // this is shared between vols and reverisons in case of piecewise reversions
		std::vector<Time> volsteptimes_;
		Array volsteptimesArray_; // FIXME this is redundant (just a copy of volsteptimes_)
		std::vector<Real> volatilities_;
		std::vector<Real> reversions_;

		boost::shared_ptr<GsrProcess> stateProcess_;
		
    };

}


#endif

